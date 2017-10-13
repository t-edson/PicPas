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
//      f_byteXbyte_byte: TxpEleFun;  //índice para función
      f_byte_mul_byte_16: TxpEleFun;  //índice para función
      f_byte_div_byte: TxpEleFun;  //índice para función
      f_word_mul_word_16: TxpEleFun;  //índice para función
      procedure byte_div_byte(fun: TxpEleFun);
      procedure byte_mul_byte_16(fun: TxpEleFun);
      procedure CopyInvert_C_to_Z;
      procedure fun_Byte(fun: TxpEleFun);
      procedure fun_DWord(fun: TxpEleFun);
      procedure Oper_bit_asig_bit(SetRes: boolean);
      procedure Oper_bit_asig_byte(SetRes: boolean);
      procedure Oper_bit_and_bit(SetRes: boolean);
      procedure Oper_bit_and_byte(SetRes: boolean);
      procedure Oper_bit_or_bit(SetRes: boolean);
      procedure Oper_bit_or_byte(SetRes: boolean);
      procedure Oper_bit_xor_bit(SetRes: boolean);
      procedure Oper_bit_xor_byte(SetRes: boolean);
      procedure Oper_bit_equ_bit(SetRes: boolean);
      procedure Oper_bit_equ_byte(SetRes: boolean);
      procedure Oper_bit_dif_bit(SetRes: boolean);
      procedure Oper_bit_dif_byte(SetRes: boolean);
      procedure Oper_byte_div_byte(SetRes: boolean);
      procedure Oper_byte_mul_byte(SetRes: boolean);
      procedure Oper_not_bit(SetRes: boolean);
      procedure Oper_not_byte(SetRes: boolean);
      procedure Oper_word_and_byte(SetRes: boolean);
      procedure Oper_word_umulword_word(SetRes: boolean);
      procedure word_mul_word_16(fun: TxpEleFun);
    private  //Operaciones con boolean
      procedure Oper_bool_asig_bool(SetRes: boolean);
      procedure Oper_not_bool(SetRes: boolean);
      procedure Oper_bool_and_bool(SetRes: boolean);
      procedure Oper_bool_or_bool(SetRes: boolean);
      procedure Oper_bool_xor_bool(SetRes: boolean);
      procedure Oper_bool_equ_bool(SetRes: boolean);
      procedure Oper_bool_dif_bool(SetRes: boolean);
    private  //Operaciones con byte
      procedure byte_oper_byte(const InstLW, InstWF: TPIC16Inst);
      procedure Oper_byte_asig_byte(SetRes: boolean);
      procedure Oper_byte_sub_byte(SetRes: boolean);
      procedure Oper_byte_add_byte(SetRes: boolean);
      procedure Oper_byte_add_word(SetRes: boolean);
      procedure Oper_byte_and_byte(SetRes: boolean);
      procedure Oper_byte_and_bit(SetRes: boolean);
      procedure Oper_byte_or_byte(SetRes: boolean);
      procedure Oper_byte_or_bit(SetRes: boolean);
      procedure Oper_byte_xor_byte(SetRes: boolean);
      procedure Oper_byte_xor_bit(SetRes: boolean);
      procedure Oper_byte_equal_byte(SetRes: boolean);
      procedure Oper_byte_difer_byte(SetRes: boolean);
      procedure Oper_byte_difer_bit(SetRes: boolean);
      procedure Oper_byte_great_byte(SetRes: boolean);
      procedure Oper_byte_less_byte(SetRes: boolean);
      procedure Oper_byte_gequ_byte(SetRes: boolean);
      procedure Oper_byte_lequ_byte(SetRes: boolean);
      procedure CodifShift_by_W(aux: TPicRegister; toRight: boolean);
      procedure Oper_byte_shr_byte(SetRes: boolean);
      procedure Oper_byte_shl_byte(SetRes: boolean);
    private  //Operaciones con Word
      procedure Oper_word_asig_word(SetRes: boolean);
      procedure Oper_word_asig_byte(SetRes: boolean);
      procedure Oper_word_equal_word(SetRes: boolean);
      procedure Oper_word_difer_word(SetRes: boolean);
      procedure Oper_word_great_word(SetRes: boolean);
      procedure Oper_word_add_word(SetRes: boolean);
      procedure Oper_word_add_byte(SetRes: boolean);
      procedure Oper_word_sub_word(SetRes: boolean);
    private
      procedure Oper_dword_aadd_dword(SetRes: boolean);
      procedure Oper_dword_add_dword(SetRes: boolean);
      procedure Oper_dword_asig_byte(SetRes: boolean);
      procedure Oper_dword_asig_dword(SetRes: boolean);
      procedure Oper_dword_asig_word(SetRes: boolean);
      procedure Oper_dword_difer_dword(SetRes: boolean);
      procedure Oper_dword_equal_dword(SetRes: boolean);
    private  //Operaciones con Char
      procedure Oper_char_asig_char(SetRes: boolean);
      procedure Oper_char_equal_char(SetRes: boolean);
      procedure Oper_char_difer_char(SetRes: boolean);
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
{Rutina genérica, que se usa antes de leer los parámetros de una función.}
begin
  {Haya o no, parámetros se debe proceder como en cualquier expresión, asumiendo que
  vamos a devolver una expresión.}
  SetResultExpres(fun.typ);  //actualiza "RTstate"
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
  _ANDWF(STATUS, toW);   //el resultado está en Z, corregido en lógica.
  InvertedFromC := true;  //Indica que se ha hecho Z = 'C. para que se pueda optimizar
end;
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
  //Guarda información de ubicación, en la ubicación actual
  pic.addPosInformation(cIn.curCon.row, cIn.curCon.col, cIn.curCon.idCtx);
end;
procedure TGenCod.expr_end(posExpres: TPosExpres);
//Se ejecuta al final de una expresión, si es que no ha habido error.
begin
  if exprLevel = 1 then begin  //el último nivel
//    Code('  ;fin expres');
  end;
  //Muestra informa
end;
function IsTheSameBitVar(var1, var2: TxpEleVar): boolean; inline;
{Indica si dos variables bit son la misma, es decir que apuntan, a la misma dirección
física}
begin
  Result := (var1.adrBit.offs = var2.adrBit.offs) and
            (var1.adrBit.bit  = var2.adrBit.bit) ;
end;
////////////operaciones con Bit
procedure TGenCod.Oper_bit_asig_bit(SetRes: boolean);
var
  dg: integer;
begin
  if p1^.Cat <> coVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.Cat of
  coConst : begin
    SetROPResultExpres_bit(operType, false);  //Realmente, el resultado no es importante
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
    SetROPResultExpres_bit(operType, false);  //Realmente, el resultado no es importante
    if IsTheSameBitVar(p1^.rVar, p2^.rVar) then begin
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
    SetROPResultExpres_bit(operType, false);  //Realmente, el resultado no es importante
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
    genError('Not implemented: "%s"', [CatOperationToStr]);
  end;
end;
procedure TGenCod.Oper_bit_asig_byte(SetRes: boolean);
begin
  if p1^.Cat <> coVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.Cat of
  coConst : begin
    SetROPResultExpres_bit(operType, false);  //Realmente, el resultado no es importante
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
    genError('Not implemented: "%s"', [CatOperationToStr]);
  end;
end;
procedure TGenCod.Oper_bit_and_bit(SetRes: boolean);
begin
    case catOperation of
    coConst_Const: begin  //AND de dos constantes. Caso especial
      SetROPResultConst_bit(p1^.valBool and p2^.valBool);
      exit;  //sale aquí, porque es un caso particular
    end;
    coConst_Variab: begin
      if p1^.valBool then begin  //p1 = 1
        //No usa ningún registro
        //Optimiza devolviendo la misma variable
        SetROPresultVariab(p2^.rVar, p2^.Inverted);  //mantiene la lógica
      end else begin   //p1 = 0
        //No usa ningún registro
        //Optimiza devolviendo constante = 0
        SetROPResultConst_bit(false);
      end;
    end;
    coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
      if p1^.valBool then begin  //p1 = 1
        //No usa ningún registro
        //Optimiza devolviendo la misma expresión en Z
        SetROPResultExpres_bit(operType, p2^.Inverted);  //mantiene la lógica
      end else begin   //p1 = 0
        //No usa ningún registro
        //Optimiza devolviendo constante = 0
        SetROPResultConst_bit(false);
        Z.used := false;  //libera el bit Z, porque ya no importa la expresión
      end;
    end;
    coVariab_Const: begin
      if p2^.valBool then begin  //p2 = 1
        //No usa ningún registro
        //Optimiza devolviendo la misma variable
        SetROPresultVariab(p1^.rVar, p1^.Inverted);  //mantiene la lógica
      end else begin   //p2 = 0
        //No usa ningún registro
        //Optimiza devolviendo constante = 0
        SetROPResultConst_bit(false);
      end;
    end;
    coVariab_Variab:begin
      if IsTheSameBitVar(p1^.rVar, p2^.rVar) then begin
        //Es la misma variable: a AND a
        //Optimiza devolviendo la misma variable
        SetROPresultVariab(p1^.rVar, p1^.Inverted);
      end else begin
        if p1^.Inverted and p2^.Inverted then begin
          //Por La ley de Morgan, se convierten em OR
          p1^.Inverted := false;
          p2^.Inverted := false;
          Oper_bit_or_bit(SetRes);  //procesa como OR
          res.Invert;   //y niega todo
          exit;
        end else if p1^.Inverted then begin
          //Este caso es lo inverso, no vale la pena implementarlo de nuevo
          ExchangeP1_P2;
          Oper_bit_and_bit(SetRes);  //procesa como OR
          exit;
        end else if p2^.Inverted then begin
          SetROPResultExpres_bit(operType, false);  //Fija resultado
          //Mueve p2 a Z
          _BANKSEL(p2^.bank);
          _MOVLW(p2^.rVar.BitMask);
          _ANDWF(p2^.offs, toW);  //Z aparece normal
          //Aplica un AND entre Z y p1,
          _BANKSEL(p1^.bank);
          _BTFSS(p1^.offs, p1^.bit);   //Si es 1, deja tal cual
          _BCF(Z.offs, Z.bit);     //Si es 0, devuelve cero
        end else begin  //Caso normal
          SetROPResultExpres_bit(operType, true);  //Fija resultado, con lógica invertida
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
        Oper_bit_or_bit(SetRes);  //procesa como OR
        exit;
      end else if p1^.Inverted then begin  //lógica invertida en p1
        SetROPResultExpres_bit(operType, false); //Fija resultado
        //Aplica un AND entre p1' y Z.
        _BANKSEL(p1^.bank);
        _BTFSC(p1^.offs, p1^.bit); //Si es 0, deja tal cual
        _BCF(Z.offs, Z.bit);      //Si es 1, devuelve cero
      end else if p2^.Inverted then begin  //lógica invertida en Z
        SetROPResultExpres_bit(operType, true); //Deja la lógica invertida por optimización
        //Aplica un AND entre p1 y Z'.
        _BANKSEL(p1^.bank);
        _BTFSS(p1^.offs, p1^.bit); //Si es 1, deja tal cual
        _BSF(Z.offs, Z.bit);       //Si es 0, devuelve cero (1, porque es lógica es invertida)
      end else begin  //lógica normal
        SetROPResultExpres_bit(operType, false); //Fija resultado
        //Aplica un AND entre p1 y Z.
        _BANKSEL(p1^.bank);
        _BTFSS(p1^.offs, p1^.bit); //Si es 1, deja tal cual
        _BCF(Z.offs, Z.bit);      //Si es 0, devuelve cero
      end;
    end;
    coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
      ExchangeP1_P2;       //Convierte en coConst_Expres
      Oper_bit_and_bit(SetRes);
      exit;
    end;
    coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
      ExchangeP1_P2;       //Convierte en coVariab_Expres
      Oper_bit_and_bit(SetRes);
      exit;
    end;
    coExpres_Expres:begin
      //la expresión p1 debe estar salvada y p2 en el acumulador
      p1^.SetAsVariab(GetVarBitFromStk);
      catOperation := TCatOperation((Ord(p1^.Cat) << 2) or ord(p2^.Cat));
      //Luego el caso es similar a variable-expresión
      Oper_bit_and_bit(SetRes);
      FreeStkRegisterBit;   //Libera pila. Ya se usó el dato.
    end;
    else
      genError('Not implemented: "%s"', [CatOperationToStr]);
    end;
end;
procedure TGenCod.Oper_bit_and_byte(SetRes: boolean);
begin
  if p2^.Cat <> coConst then begin
    GenError('Incompatible types: (bit) AND (byte).'); exit;
  end;
  //p2 es constante
  if p2^.valInt = 0 then begin
    p2^.SetAsConst(typBit);   //convierte en bit
    p2^.valBool := false;
    Oper_bit_and_bit(SetRes);  //opera como bit
  end else if p2^.valInt = 1 then begin
    p2^.SetAsConst(typBit);   //convierte en bit
    p2^.valBool := true;
    Oper_bit_and_bit(SetRes);  //opera como bit
  end else begin
    GenError('Incompatible types: (bit) AND (byte).'); exit;
  end;
end;
procedure TGenCod.Oper_bit_or_bit(SetRes: boolean);
begin
    case catOperation of
    coConst_Const: begin  //AND de dos constantes. Caso especial
      SetROPResultConst_bit(p1^.valBool or p2^.valBool);
      exit;  //sale aquí, porque es un caso particular
    end;
    coConst_Variab: begin
      if p1^.valBool then begin  //p1 = 1
        //No usa ningún registro
        //Optimiza devolviendo constante = 1
        SetROPResultConst_bit(true);
      end else begin   //p1 = 0
        //No usa ningún registro
        //Optimiza devolviendo la misma variable
        SetROPresultVariab(p2^.rVar, p2^.Inverted);
      end;
    end;
    coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
      if p1^.valBool then begin  //p1 = 1
        //No usa ningún registro
        //Optimiza devolviendo constante = 1
        SetROPResultConst_bit(true);
        Z.used := false;  //libera el bit Z, porque ya no importa la expresión
      end else begin   //p1 = 0
        //No usa ningún registro
        //Optimiza devolviendo la misma expresión en Z
        SetROPResultExpres_bit(operType, p2^.Inverted);  //mantiene la lógica
      end;
    end;
    coVariab_Const: begin
      if p2^.valBool then begin  //p2 = 1
        //No usa ningún registro
        //Optimiza devolviendo constante = 1
        SetROPResultConst_bit(true);
      end else begin   //p2 = 0
        //No usa ningún registro
        //Optimiza devolviendo la misma variable
        SetROPresultVariab(p1^.rVar, p1^.Inverted);
      end;
    end;
    coVariab_Variab:begin
      if IsTheSameBitVar(p1^.rVar, p2^.rVar) then begin
        //Es la misma variable: a OR a
        //Optimiza devolviendo la misma variable
        SetROPresultVariab(p1^.rVar, p1^.Inverted);
      end else begin
        if p1^.Inverted and p2^.Inverted then begin
          //Por La ley de Morgan, se convierten em AND
          p1^.Inverted := false;
          p2^.Inverted := false;
          Oper_bit_and_bit(SetRes);  //procesa como OR
          res.Invert;
          exit;
        end else if p1^.Inverted then begin
          //Este caso es lo inverso, no vale la pena implementarlo de nuevo
          ExchangeP1_P2;
          Oper_bit_or_bit(SetRes);  //procesa como OR
          exit;
        end else if p2^.Inverted then begin
          SetROPResultExpres_bit(operType, false);  //Fija resultado
          //Mueve p2 a Z
          _BANKSEL(p2^.bank);
          _MOVLW(p2^.rVar.BitMask);
          _ANDWF(p2^.offs, toW);  //Z aparece normal
          //Aplica un OR entre Z y p1,
          _BANKSEL(p1^.bank);
          _BTFSC(p1^.offs, p1^.bit);   //Si es 0, deja tal cual
          _BSF(Z.offs, Z.bit);     //Si es 1, devuelve uno
        end else begin  //Caso normal
          SetROPResultExpres_bit(operType, true);  //Fija resultado, con lógica invertida
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
        Oper_bit_and_bit(SetRes);  //procesa como OR
        exit;
      end else if p1^.Inverted then begin  //lógica invertida
        SetROPResultExpres_bit(operType, false);  //Fija resultado
        //Aplica un OR entre p1' y Z.
        _BANKSEL(p1^.bank);
        _BTFSS(p1^.offs, p1^.bit);   //Si es 1, deja tal cual
        _BSF(Z.offs, Z.bit);     //Si es 0, devuelve uno
      end else if p2^.Inverted then begin  //lógica invertida en Z
        SetROPResultExpres_bit(operType, true); //Deja la lógica invertida por optimización
        //Aplica un OR entre p1 y Z.
        _BANKSEL(p1^.bank);
        _BTFSC(p1^.offs, p1^.bit);   //Si es 0, deja tal cual
        _BCF(Z.offs, Z.bit);     //Si es 1, devuelve uno (0 porque es lógica invertida)
      end else begin   //lógica normal
        SetROPResultExpres_bit(operType, false);  //Fija resultado
        //Aplica un OR entre p1 y Z.
        _BANKSEL(p1^.bank);
        _BTFSC(p1^.offs, p1^.bit);   //Si es 0, deja tal cual
        _BSF(Z.offs, Z.bit);     //Si es 1, devuelve uno
      end;
    end;
    coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
      ExchangeP1_P2;       //Convierte en coConst_Expres
      Oper_bit_or_bit(SetRes);
      exit;
    end;
    coExpres_Variab:begin  //la expresión p2 se evaluó y esta en W
      ExchangeP1_P2;       //Convierte en coVariab_Expres
      Oper_bit_or_bit(SetRes);
      exit;
    end;
    coExpres_Expres:begin
      //la expresión p1 debe estar salvada y p2 en el acumulador
      p1^.SetAsVariab(GetVarBitFromStk);
      catOperation := TCatOperation((Ord(p1^.Cat) << 2) or ord(p2^.Cat));
      //Luego el caso es similar a variable-expresión
      Oper_bit_or_bit(SetRes);
      FreeStkRegisterBit;   //Libera pila. Ya se usó el dato.
    end;
    else
      genError('Not implemented: "%s"', [CatOperationToStr]);
    end;
end;
procedure TGenCod.Oper_bit_or_byte(SetRes: boolean);
begin
  if p2^.Cat <> coConst then begin
    GenError('Incompatible types: (bit) OR (byte).'); exit;
  end;
  //p2 es constante
  if p2^.valInt = 0 then begin
    p2^.SetAsConst(typBit);   //convierte en bit
    p2^.valBool := false;
    Oper_bit_or_bit(SetRes);  //opera como bit
  end else if p2^.valInt = 1 then begin
    p2^.SetAsConst(typBit);   //convierte en bit
    p2^.valBool := true;
    Oper_bit_or_bit(SetRes);  //opera como bit
  end else begin
    GenError('Incompatible types: (bit) OR (byte).'); exit;
  end;
end;
procedure TGenCod.Oper_bit_xor_bit(SetRes: boolean);
begin
    case catOperation of
    coConst_Const: begin  //XOR de dos constantes. Caso especial
      SetROPResultConst_bit(p1^.valBool xor p2^.valBool);
      exit;  //sale aquí, porque es un caso particular
    end;
    coConst_Variab: begin
      if p1^.valBool then begin  //p1 = 1
        //Optimiza devolviendo la variable invertida
        SetROPresultVariab(p2^.rVar, not p2^.Inverted);
      end else begin   //p1 = 0
        //Optimiza devolviendo la misma variable
        SetROPresultVariab(p2^.rVar, p2^.Inverted);
      end;
    end;
    coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
      if p1^.valBool then begin  //p1 = 1
        //Optimiza devolviendo la expresión invertida
        SetROPResultExpres_bit(operType, not p2^.Inverted);  //mantiene la lógica
      end else begin   //p1 = 0
        //Optimiza devolviendo la misma expresión en Z
        SetROPResultExpres_bit(operType, p2^.Inverted);  //mantiene la lógica
      end;
    end;
    coVariab_Const: begin
      ExchangeP1_P2;  //Convierte a coConst_Variab
      Oper_bit_xor_bit(SetRes);
      exit;
    end;
    coVariab_Variab:begin
      if IsTheSameBitVar(p1^.rVar, p2^.rVar) then begin
        //Es la misma variable: a XOR a
        //Optimiza devolviendo cero
        SetROPResultConst_bit(false);
      end else begin
        if p1^.Inverted and p2^.Inverted then begin
          p1^.Inverted := false;
          p2^.Inverted := false;
          Oper_bit_xor_bit(SetRes);  //es lo mismo
          exit;
        end else if p1^.Inverted then begin
          //Este caso es lo inverso, no vale la pena implementarlo de nuevo
          ExchangeP1_P2;
          Oper_bit_xor_bit(SetRes);  //procesa como OR
          exit;
        end else if p2^.Inverted then begin
          //a XOR b' = (z XOR b)'
          p2^.Inverted := false;
          Oper_bit_xor_bit(SetRes);
          res.Invert;  //Invierte la lógica
          exit;
        end else begin  //Caso normal
          {Se optimiza bien, esta operación, porque es una rutina muy usada para loa
          las operaciones XOR, y porque también se utiliza el XOR para las comparaciones
          de bits.}
          if p1^.bit = p2^.bit then begin
            //Están en el mismo bit, se puede optimizar
            SetROPResultExpres_bit(operType, true);  //Fija resultado
            _BANKSEL(p2^.bank);
            _MOVF(p2^.offs, toW);  //mueve a W
            _BANKSEL(p1^.bank);
            _XORWF(p1^.offs, toW);      //APlica XOR,
            _ANDLW(p1^.rVar.BitMask);  //Aplica máscara al bit que nos interesa, queda en Z, invertido
          end else if p1^.bit = p2^.bit +1 then begin
            //p1 está a un bit a la izquierda, se puede optimizar
            SetROPResultExpres_bit(operType, true);  //Fija resultado
            _BANKSEL(p2^.bank);
            _RLF(p2^.offs, toW);  //alinea y mueve a W
            _BANKSEL(p1^.bank);
            _XORWF(p1^.offs, toW);      //APlica XOR,
            _ANDLW(p1^.rVar.BitMask);  //Aplica máscara al bit que nos interesa, queda en Z, invertido
          end else if p1^.bit = p2^.bit-1 then begin
            //p1 está a un bit a la derecha, se puede optimizar
            SetROPResultExpres_bit(operType, true);  //Fija resultado
            _BANKSEL(p2^.bank);
            _RRF(p2^.offs, toW);  //alinea y mueve a W
            _BANKSEL(p1^.bank);
            _XORWF(p1^.offs, toW);      //APlica XOR,
            _ANDLW(p1^.rVar.BitMask);  //Aplica máscara al bit que nos interesa, queda en Z, invertido
          end else if abs(p1^.bit - p2^.bit) = 4 then begin
            //p1 está a un nibble de distancia, se puede optimizar
            SetROPResultExpres_bit(operType, true);  //Fija resultado
            _BANKSEL(p2^.bank);
            _SWAPF(p2^.offs, toW);  //alinea y mueve a W
            _BANKSEL(p1^.bank);
            _XORWF(p1^.offs, toW);      //APlica XOR,
            _ANDLW(p1^.rVar.BitMask);  //Aplica máscara al bit que nos interesa, queda en Z, invertido
          end else begin
            //La forma larga
            SetROPResultExpres_bit(operType, false);  //Fija resultado,
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
        Oper_bit_xor_bit(SetRes);   //es lo mismo
        exit;
      end else if p1^.Inverted then begin  //lógica invertida
        SetROPResultExpres_bit(operType, false);  //Fija resultado
        //Aplica un XOR entre p1' y Z.
        _BANKSEL(p1^.bank);
        _MOVLW($1 << Z.bit);   //carga máscara, y deja lista si es eu se necesita
        _BTFSS(p1^.offs, p1^.bit);   //Si es 1(0), deja tal cual
        _ANDWF(Z.offs, toW);     //Si es 0(1), invierte
      end else if p2^.Inverted then begin  //lógica invertida en Z
        SetROPResultExpres_bit(operType, false);  //Fija resultado
        //Aplica un XOR entre p1 y Z'.
        _BANKSEL(p1^.bank);
        _MOVLW($1 << Z.bit);   //carga máscara, y deja lista si es eu se necesita
        _BTFSS(p1^.offs, p1^.bit);   //Si es 1, invierte (deja igual porque ya está invertido)
        _ANDWF(Z.offs, toW);     //Si es 0, deja tal cual (realmente debe invertir)
      end else begin   //lógica normal
        SetROPResultExpres_bit(operType, false);  //Fija resultado
        //Aplica un XOR entre p1 y Z.
        _BANKSEL(p1^.bank);
        _MOVLW($1 << Z.bit);   //carga máscara, y deja lista si es eu se necesita
        _BTFSC(p1^.offs, p1^.bit);  //Si es 0, deja tal cual
        _ANDWF(Z.offs, toW);         //Si es 1, invierte
      end;
    end;
    coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
      ExchangeP1_P2;       //Convierte en coConst_Expres
      Oper_bit_xor_bit(SetRes);
      exit;
    end;
    coExpres_Variab:begin  //la expresión p2 se evaluó y esta en W
      ExchangeP1_P2;       //Convierte en coVariab_Expres
      Oper_bit_xor_bit(SetRes);
      exit;
    end;
    coExpres_Expres:begin
      //la expresión p1 debe estar salvada y p2 en el acumulador
      p1^.SetAsVariab(GetVarBitFromStk);
      catOperation := TCatOperation((Ord(p1^.Cat) << 2) or ord(p2^.Cat));
      //Luego el caso es similar a coVariab_Expres
      Oper_bit_xor_bit(SetRes);
      FreeStkRegisterBit;   //Libera pila. Ya se usó el dato.
    end;
    else
      genError('Not implemented: "%s"', [CatOperationToStr]);
    end;
end;
procedure TGenCod.Oper_bit_xor_byte(SetRes: boolean);
begin
  if p2^.Cat <> coConst then begin
    GenError('Incompatible types: (bit) XOR (byte).'); exit;
  end;
  //p2 es constante
  if p2^.valInt = 0 then begin
    p2^.SetAsConst(typBit);   //convierte en bit
    p2^.valBool := false;
    Oper_bit_xor_bit(SetRes);  //opera como bit
  end else if p2^.valInt = 1 then begin
    p2^.SetAsConst(typBit);   //convierte en bit
    p2^.valBool := true;
    Oper_bit_xor_bit(SetRes);  //opera como bit
  end else begin
    GenError('Incompatible types: (bit) XOR (byte).'); exit;
  end;
end;
procedure TGenCod.Oper_bit_equ_bit(SetRes: boolean);
begin
  //Una comparación, es lo mismo que un XOR negado
  Oper_bit_xor_bit(SetRes);  //puede devolver error
  //Niega la lógica
  res.Invert;  //Invierte la lógica
  ChangeResultBitToBool;  //devuelve boolean
end;
procedure TGenCod.Oper_bit_equ_byte(SetRes: boolean);
begin
  //Una comparación, es lo mismo que un XOR negado
  Oper_bit_xor_byte(SetRes);  //puede devolver error
  //¿Y si devuelve variable?
  res.Invert;  //Invierte la lógica
  ChangeResultBitToBool;  //devuelve boolean
end;
procedure TGenCod.Oper_bit_dif_bit(SetRes: boolean);
begin
  //Esta comparación, es lo mismo que un XOR
  Oper_bit_xor_bit(SetRes);  //puede devolver error
  ChangeResultBitToBool;  //devuelve boolean
end;
procedure TGenCod.Oper_bit_dif_byte(SetRes: boolean);
begin
  //Una comparación, es lo mismo que un XOR
  Oper_bit_xor_byte(SetRes);  //puede devolver error
  ChangeResultBitToBool;  //devuelve boolean
end;
procedure TGenCod.Oper_not_bit(SetRes: boolean);
begin
  case p1^.Cat of
  coConst : begin
    {Actualmente no existen constantes de tipo "Bit", pero si existieran, sería así}
    SetROPResultConst_bit(not p1^.valBool);
  end;
  coVariab: begin
    {Optimiza devolviendo la misma variable, pero invirtiendo la lógica.}
    SetROPresultVariab(p1^.rVar, not p1^.Inverted);
  end;
  coExpres: begin  //ya está en STATUS.Z
    //No cambiamos su valor, sino su significado.
    SetROPResultExpres_bit(operType, not p1^.Inverted);
  end;
  else
    genError('Not implemented: "%s"', [CatOperationToStr]);
  end;
end;
procedure TGenCod.Oper_not_byte(SetRes: boolean);
begin
  case p1^.Cat of
  coConst : begin
    {Actualmente no existen constantes de tipo "Bit", pero si existieran, sería así}
    SetROPResultConst_byte((not p1^.valInt) and $FF);
  end;
  coVariab: begin
    SetROPResultExpres_byte(operType);
    _COMF(p1^.offs, toW);
  end;
//  coExpres: begin
//    SetROPResultExpres_byte(operType);
//    //////
//  end;
  else
    genError('Not implemented: "%s"', [CatOperationToStr]);
  end;
end;
////////////operaciones con Boolean
procedure TGenCod.Oper_bool_asig_bool(SetRes: boolean);
begin
  Oper_bit_asig_bit(SetRes);  //A bajo nivel es lo mismo
end;
procedure TGenCod.Oper_not_bool(SetRes: boolean);
begin
  Oper_not_bit(SetRes);  //A bajo nivel es lo mismo
  ChangeResultBitToBool;  //pero debe devolver este tipo
end;
procedure TGenCod.Oper_bool_and_bool(SetRes: boolean);
begin
  Oper_bit_and_bit(SetRes);  //A bajo nivel es lo mismo
  ChangeResultBitToBool;  //pero debe devolver este tipo
end;
procedure TGenCod.Oper_bool_or_bool(SetRes: boolean);
begin
  Oper_bit_or_bit(SetRes);  //A bajo nivel es lo mismo
  ChangeResultBitToBool;  //pero debe devolver este tipo
end;
procedure TGenCod.Oper_bool_xor_bool(SetRes: boolean);
begin
  Oper_bit_xor_bit(SetRes);  //A bajo nivel es lo mismo
  ChangeResultBitToBool;  //pero debe devolver este tipo
end;
procedure TGenCod.Oper_bool_equ_bool(SetRes: boolean);
begin
  Oper_bit_equ_bit(SetRes);  //Es lo mismo
end;
procedure TGenCod.Oper_bool_dif_bool(SetRes: boolean);
begin
  Oper_bit_dif_bit(SetRes);
end;
//////////// Operaciones con Byte
procedure TGenCod.Oper_byte_asig_byte(SetRes: boolean);
begin
  if p1^.Cat <> coVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.Cat of
  coConst : begin
    SetROPResultExpres_byte(operType);  //Realmente, el resultado no es importante
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
    SetROPResultExpres_byte(operType);  //Realmente, el resultado no es importante
    _BANKSEL(p2^.bank);  //verifica banco fuente
    _MOVF(p2^.offs, toW);
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVWF(p1^.offs);
  end;
  coExpres: begin  //ya está en w
    SetROPResultExpres_byte(operType);  //Realmente, el resultado no es importante
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
  rVar: TxpEleVar;
begin
  case catOperation of
  coConst_Variab: begin
    SetROPResultExpres_byte(operType);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    CodAsmK(InstLW, p1^.valInt);  //deja en W
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    SetROPResultExpres_byte(operType);
    CodAsmK(InstLW, p1^.valInt);  //deja en W
  end;
  coVariab_Const: begin
    SetROPResultExpres_byte(operType);
    _MOVLW(p2^.valInt);
    _BANKSEL(p1^.bank);
    CodAsmFD(InstWF, p1^.offs, toW);  //deja en W
  end;
  coVariab_Variab:begin
    SetROPResultExpres_byte(operType);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _BANKSEL(p1^.bank);
    CodAsmFD(InstWF, p1^.offs, toW);  //deja en W
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROPResultExpres_byte(operType);
    _BANKSEL(p1^.bank);
    CodAsmFD(InstWF, p1^.offs, toW);  //deja en W
  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetROPResultExpres_byte(operType);
    CodAsmK(InstLW, p2^.valInt);  //deja en W
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROPResultExpres_byte(operType);
    _BANKSEL(p2^.bank);
    CodAsmFD(InstWF, p2^.offs, toW);  //deja en W
  end;
  coExpres_Expres:begin
    SetROPResultExpres_byte(operType);
    //La expresión p1 debe estar salvada y p2 en el acumulador
    rVar := GetVarByteFromStk;
    _BANKSEL(rVar.adrByte0.bank);
    CodAsmFD(InstWF, rVar.adrByte0.offs, toW);  //opera directamente al dato que había en la pila. Deja en W
    FreeStkRegisterByte;   //libera pila porque ya se uso
  end;
  end;
end;
procedure TGenCod.Oper_byte_add_byte(SetRes: boolean);
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    SetROPResultConst_byte(p1^.valInt+p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    byte_oper_byte(ADDLW, ADDWF);
end;
procedure TGenCod.Oper_byte_add_word(SetRes: boolean);
begin
  case catOperation of
  coExpres_Expres:begin
    {Este es el único caso que no se puede invertir, por la posición de los operandos en
     la pila.}
    //la expresión p1 debe estar salvada y p2 en el acumulador
    p1^.SetAsVariab(GetVarByteFromStk);  //Convierte a variable
    catOperation := TCatOperation((Ord(p1^.Cat) << 2) or ord(p2^.Cat));
    //Luego el caso es similar a coVariab_Expres
    Oper_byte_add_word(SetRes);
    FreeStkRegisterByte;   //libera pila porque ya se usó el dato ahí contenido
  end;
  else
    //Para los otros casos, funciona
    ExchangeP1_P2;   //Invierte los operandos
    Oper_word_add_byte(SetRes); //Y llama a la función opuesta
  end;
end;
procedure TGenCod.Oper_byte_sub_byte(SetRes: boolean);
var
  rVar: TxpEleVar;
begin
  case catOperation of
  coConst_Const:begin  //suma de dos constantes. Caso especial
    SetROPResultConst_byte(p1^.valInt-p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  coConst_Variab: begin
    SetROPResultExpres_byte(operType);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _SUBLW(p1^.valInt);   //K - W -> W
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    SetROPResultExpres_byte(operType);
    _SUBLW(p1^.valInt);   //K - W -> W
  end;
  coVariab_Const: begin
    SetROPResultExpres_byte(operType);
    _MOVLW(p2^.valInt);
    _BANKSEL(p1^.bank);
    _SUBWF(p1^.offs, toW);  //F - W -> W
  end;
  coVariab_Variab:begin
    SetROPResultExpres_byte(operType);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _BANKSEL(p1^.bank);
    _SUBWF(p1^.offs, toW);  //F - W -> W
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROPResultExpres_byte(operType);
    _BANKSEL(p1^.bank);
    _SUBWF(p1^.offs, toW);  //F - W -> W
  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetROPResultExpres_byte(operType);
    _SUBLW(p2^.valInt);  //K - W -> W
    _SUBLW(0);  //K - W -> W   //invierte W
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROPResultExpres_byte(operType);
    _BANKSEL(p2^.bank);
    _SUBWF(p2^.offs, toW);  //F - W -> W
    _SUBLW(0);  //K - W -> W   //invierte W
  end;
  coExpres_Expres:begin
    SetROPResultExpres_byte(operType);
    //la expresión p1 debe estar salvada y p2 en el acumulador
    rVar := GetVarByteFromStk;
    _BANKSEL(rVar.adrByte0.bank);
    _SUBWF(rVar.adrByte0.offs, toW);  //opera directamente al dato que había en la pila. Deja en W
    FreeStkRegisterByte;   //libera pila porque ya se uso
  end;
  end;
end;
procedure TGenCod.byte_mul_byte_16(fun: TxpEleFun);
//E * W -> [H:W]  Usa registros: W,H,E,U
//Basado en código de Andrew Warren http://www.piclist.com
var
  LOOP: Word;
begin
    typDWord.DefineRegister;   //Asegura que exista W,H,E,U
    _CLRF (H.offs);
    _CLRF (U.offs);
    _BSF  (U.offs,3);  //8->U
    _RRF  (E.offs,toF);
LOOP:=_PC;
    _BTFSC (STATUS,0);
    _ADDWF (H.offs,toF);
    _RRF   (H.offs,toF);
    _RRF   (E.offs,toF);
    _DECFSZ(U.offs, toF);
    _GOTO  (LOOP);
    //Realmente el algortimo es: E*W -> [H:E], pero lo convertimos a: E*W -> [H:W]
    _MOVF(E.offs, toW);
    _RETURN;
end;
procedure TGenCod.Oper_byte_mul_byte(SetRes: boolean);
var
  rVar: TxpEleVar;
begin
  case catOperation of
  coConst_Const:begin  //producto de dos constantes. Caso especial
    SetROPResultConst_word((p1^.valInt*p2^.valInt) and $FFFF);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  coConst_Variab: begin
    if p1^.valInt=0 then begin  //caso especial
      SetROPResultConst_byte(0);
      exit;
    end else if p1^.valInt=1 then begin  //caso especial
      SetROPresultVariab(p2^.rVar);
      exit;
    end else if p1^.valInt=2 then begin
      SetROPResultExpres_word(operType);
      _BANKSEL(H.bank);
      _CLRF(H.offs);
      _BCF(STATUS, _C);
      _BANKSEL(P2^.bank);
      _RLF(p2^.offs, toW);
      _BANKSEL(H.bank);
      _RLF(H.offs, toF);
      exit;
    end;
    SetROPResultExpres_word(operType);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _BANKSEL(E.bank);
    _MOVWF(E.offs);
    _MOVLW(p1^.valInt);
    _CALL(f_byte_mul_byte_16.adrr);
    if FirstPass then f_byte_mul_byte_16.AddCaller;
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    SetROPResultExpres_word(operType);
    _BANKSEL(E.bank);
    _MOVWF(E.offs);
    _MOVLW(p1^.valInt);
    _CALL(f_byte_mul_byte_16.adrr);
    if FirstPass then f_byte_mul_byte_16.AddCaller;
  end;
  coVariab_Const: begin
    SetROPResultExpres_word(operType);
    _BANKSEL(p1^.bank);
    _MOVF(p1^.offs, toW);
    _BANKSEL(E.bank);
    _MOVWF(E.offs);
    _MOVLW(p2^.valInt);
    _CALL(f_byte_mul_byte_16.adrr);
    if FirstPass then f_byte_mul_byte_16.AddCaller;
  end;
  coVariab_Variab:begin
    SetROPResultExpres_word(operType);
    _BANKSEL(p1^.bank);
    _MOVF(p1^.offs, toW);
    _BANKSEL(E.bank);
    _MOVWF(E.offs);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _CALL(f_byte_mul_byte_16.adrr);
    if FirstPass then f_byte_mul_byte_16.AddCaller;
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROPResultExpres_word(operType);
    _BANKSEL(E.bank);
    _MOVWF(E.offs);  //p2 -> E
    _BANKSEL(p1^.bank);
    _MOVF(p1^.offs, toW); //p1 -> W
    _CALL(f_byte_mul_byte_16.adrr);
    if FirstPass then f_byte_mul_byte_16.AddCaller;
  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetROPResultExpres_word(operType);
    _BANKSEL(E.bank);
    _MOVWF(E.offs);  //p1 -> E
    _MOVLW(p2^.valInt); //p2 -> W
    _CALL(f_byte_mul_byte_16.adrr);
    if FirstPass then f_byte_mul_byte_16.AddCaller;
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROPResultExpres_word(operType);
    _BANKSEL(E.bank);
    _MOVWF(E.offs);  //p1 -> E
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW); //p2 -> W
    _CALL(f_byte_mul_byte_16.adrr);
    if FirstPass then f_byte_mul_byte_16.AddCaller;
  end;
  coExpres_Expres:begin
    SetROPResultExpres_word(operType);
    //la expresión p1 debe estar salvada y p2 en el acumulador
    rVar := GetVarByteFromStk;
    _BANKSEL(E.bank);
    _MOVWF(E.offs);  //p2 -> E
    _BANKSEL(rVar.adrByte0.bank);
    _MOVF(rVar.adrByte0.offs, toW); //p1 -> W
    _CALL(f_byte_mul_byte_16.adrr);
    FreeStkRegisterByte;   //libera pila porque se usará el dato ahí contenido
    {Se podría ahorrar el paso de mover la variable de la pila a W (y luego a una
    variable) temporal, si se tuviera una rutina de multiplicación que compilara a
    partir de la direccion de una variable (en este caso de la pila, que se puede
    modificar), pero es un caso puntual, y podría no reutilizar el código apropiadamente.}
    if FirstPass then f_byte_mul_byte_16.AddCaller;
  end;
  end;
end;
procedure TGenCod.byte_div_byte(fun: TxpEleFun);
//H div W -> E  Usa registros: W,H,E,U
//H mod W -> U  Usa registros: W,H,E,U
//Basado en código del libro "MICROCONTROLADOR PIC16F84. DESARROLLO DE PROYECTOS" E. Palacios, F. Remiro y L.J. López
var
  Arit_DivideBit8: Word;
  aux, aux2: TPicRegister;
begin
    typDWord.DefineRegister;   //Asegura que exista W,H,E,U
    aux := GetAuxRegisterByte;  //Pide registro auxiliar
//    aux2 := GetAuxRegisterByte;  //Pide registro auxiliar
    aux2 := FSR;   //utiliza FSR como registro auxiliar
    _MOVWF (aux.offs);
    _clrf   (E.offs);        //En principio el resultado es cero.
    _clrf   (U.offs);
    _movlw  (8);             //Carga el contador.
    _movwf  (aux2.offs);
Arit_DivideBit8 := _PC;
    _rlf    (H.offs,toF);
    _rlf    (U.offs,toF);    // (U.offs) contiene el dividendo parcial.
    _movf   (aux.offs,toW);
    _subwf  (U.offs,toW);    //Compara dividendo parcial y divisor.
    _btfsc  (STATUS,_C);     //Si (dividendo parcial)>(divisor)
    _movwf  (U.offs);        //(dividendo parcial) - (divisor) --> (dividendo parcial)
    _rlf    (E.offs,toF);    //Desplaza el cociente introduciendo el bit apropiado.
    _decfsz (aux2.offs,toF);
    _goto   (Arit_DivideBit8);
    _movf   (E.offs,toW);    //Devuelve también en (W)
    _RETURN;
//    aux2.used := false;
    aux.used := false;
end;
procedure TGenCod.Oper_byte_div_byte(SetRes: boolean);
var
  rVar: TxpEleVar;
begin
  case catOperation of
  coConst_Const:begin  //producto de dos constantes. Caso especial
    if p2^.valInt = 0 then begin
      GenError('Cannot divide by zero');
      exit;
    end;
    SetROPResultConst_word(p1^.valInt div p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  coConst_Variab: begin
    if p1^.valInt=0 then begin  //caso especial
      SetROPResultConst_byte(0);
      exit;
    end;
    SetROPResultExpres_byte(operType);
    _MOVLW(p1^.valInt);
    _BANKSEL(H.bank);
    _MOVWF(H.offs);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _CALL(f_byte_div_byte.adrr);
    if FirstPass then f_byte_div_byte.AddCaller;
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    if p1^.valInt=0 then begin  //caso especial
      SetROPResultConst_byte(0);
      exit;
    end;
    SetROPResultExpres_byte(operType);
    _BANKSEL(E.bank);
    _MOVWF(E.offs);  //guarda divisor

    _MOVLW(p1^.valInt);
    _BANKSEL(H.bank);
    _MOVWF(H.offs);  //dividendo

    _BANKSEL(E.bank);
    _MOVF(E.offs, toW);  //divisor
    _CALL(f_byte_div_byte.adrr);
    if FirstPass then f_byte_div_byte.AddCaller;
  end;
  coVariab_Const: begin
    SetROPResultExpres_byte(operType);
    _BANKSEL(p1^.bank);
    _MOVF(p1^.offs, toW);
    _BANKSEL(H.bank);
    _MOVWF(H.offs);
    _MOVLW(p2^.valInt);
    _CALL(f_byte_div_byte.adrr);
    if FirstPass then f_byte_div_byte.AddCaller;
  end;
  coVariab_Variab:begin
    SetROPResultExpres_byte(operType);
    _BANKSEL(p1^.bank);
    _MOVF(p1^.offs, toW);
    _BANKSEL(H.bank);
    _MOVWF(H.offs);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _CALL(f_byte_div_byte.adrr);
    if FirstPass then f_byte_div_byte.AddCaller;
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROPResultExpres_byte(operType);
    //guarda divisor
    _BANKSEL(E.bank);
    _MOVWF(E.offs);
    //p1 -> H
    _BANKSEL(p1^.bank);
    _MOVF(p1^.offs, toW); //p1 -> W
    _BANKSEL(H.bank);
    _MOVWF(H.offs);  //dividendo

    _BANKSEL(E.bank);
    _MOVF(E.offs, toW);  //divisor
    _CALL(f_byte_div_byte.adrr);
    if FirstPass then f_byte_div_byte.AddCaller;
  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetROPResultExpres_byte(operType);
    _BANKSEL(H.bank);
    _MOVWF(H.offs);  //p1 -> H
    _MOVLW(p2^.valInt); //p2 -> W
    _CALL(f_byte_div_byte.adrr);
    if FirstPass then f_byte_div_byte.AddCaller;
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROPResultExpres_byte(operType);
    _BANKSEL(H.bank);
    _MOVWF(H.offs);  //p1 -> H
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW); //p2 -> W
    _CALL(f_byte_div_byte.adrr);
    if FirstPass then f_byte_div_byte.AddCaller;
  end;
  coExpres_Expres:begin
    SetROPResultExpres_byte(operType);
    //la expresión p1 debe estar salvada y p2 en el acumulador
    rVar := GetVarByteFromStk;
    //guarda divisor
    _BANKSEL(E.bank);
    _MOVWF(E.offs);
    //pila -> H
    _BANKSEL(rVar.adrByte0.bank);
    _MOVF(rVar.adrByte0.offs, toW); //p1 -> W
    _BANKSEL(H.bank);
    _MOVWF(H.offs);  //dividendo
    //divisor -> W
    _BANKSEL(E.bank);
    _MOVF(E.offs, toW);  //p2 -> E

    _CALL(f_byte_div_byte.adrr);
    FreeStkRegisterByte;   //libera pila porque se usará el dato ahí contenido
    {Se podría ahorrar el paso de mover la variable de la pila a W (y luego a una
    variable) temporal, si se tuviera una rutina de multiplicación que compilara a
    partir de la direccion de una variable (en este caso de la pila, que se puede
    modificar), pero es un caso puntual, y podría no reutilizar el código apropiadamente.}
    if FirstPass then f_byte_div_byte.AddCaller;
  end;
  end;
end;
procedure TGenCod.Oper_byte_and_byte(SetRes: boolean);
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    SetROPResultConst_byte(p1^.valInt and p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    byte_oper_byte(ANDLW, ANDWF);
end;
procedure TGenCod.Oper_byte_and_bit(SetRes: boolean);
begin
  {No hay problema en usar siempre ExchangeP1_P2, porque el caso Expresión-Expresión,
  no se implementa Oper_bit_and_byte.}
  ExchangeP1_P2;   //Invierte los operandos
  Oper_bit_and_byte(SetRes);
end;
procedure TGenCod.Oper_byte_or_byte(SetRes: boolean);
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    SetROPResultConst_byte(p1^.valInt or p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    byte_oper_byte(IORLW, IORWF);
end;
procedure TGenCod.Oper_byte_or_bit(SetRes: boolean);
begin
  {No hay problema en usar siempre ExchangeP1_P2, porque el caso Expresión-Expresión,
  no se implementa Oper_bit_or_byte.}
  ExchangeP1_P2;   //Invierte los operandos
  Oper_bit_or_byte(SetRes);
end;
procedure TGenCod.Oper_byte_xor_byte(SetRes: boolean);
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    SetROPResultConst_byte(p1^.valInt xor p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    byte_oper_byte(XORLW, XORWF);
end;
procedure TGenCod.Oper_byte_xor_bit(SetRes: boolean);
begin
  {No hay problema en usar siempre ExchangeP1_P2, porque el caso Expresión-Expresión,
  no se implementa Oper_bit_xor_byte.}
  ExchangeP1_P2;   //Invierte los operandos
  Oper_bit_xor_byte(SetRes);
end;
procedure TGenCod.Oper_byte_equal_byte(SetRes: boolean);
var
  rVar: TxpEleVar;
begin
  case catOperation of
  coConst_Const: begin  //compara constantes. Caso especial
    SetROPResultConst_bool(p1^.valInt = p2^.valInt);
  end;
  coConst_Variab: begin
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
    if p1^.valInt = 0 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _MOVF(p2^.offs, toF);  //si iguales _Z=1
    end else if p1^.valInt = 1 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _DECF(p2^.offs, toW);  //si el resultado es cero _Z=1
    end else if p1^.valInt = 255 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _INCF(p2^.offs, toW);  //si el resultado es cero _Z=1
    end else begin
      _MOVLW(p1^.valInt);
      _BANKSEL(p2^.bank);  //verifica banco destino
      _SUBWF(p2^.offs, toW);  //si iguales _Z=1
    end;
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
    _SUBLW(p1^.valInt);  //si iguales _Z=1
  end;
  coVariab_Const: begin
    ExchangeP1_P2;  //Convierte a coConst_Variab
    Oper_byte_equal_byte(SetRes);
  end;
  coVariab_Variab:begin
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.offs, toW);
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.offs, toW);  //si iguales _Z=1
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
    //ReserveW; if HayError then exit;
    _BANKSEL(p1^.bank);  //verifica banco destino
    _SUBWF(p1^.offs, toW);  //si iguales _Z=1
  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
    //ReserveW; if HayError then exit;
    _SUBLW(p2^.valInt);  //si iguales _Z=1
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
    //ReserveW; if HayError then exit;
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.offs, toW);  //si iguales _Z=1
  end;
  coExpres_Expres:begin
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
    //la expresión p1 debe estar salvada y p2 en el acumulador
    rVar := GetVarByteFromStk;
    _BANKSEL(rVar.adrByte0.bank);  //verifica banco destino
    _SUBWF(rVar.adrByte0.offs, toW);  //compara directamente a lo que había en pila.
    FreeStkRegisterByte;   //libera pila porque se usará el dato ahí contenido
  end;
  end;
end;
procedure TGenCod.Oper_byte_difer_byte(SetRes: boolean);
begin
  Oper_byte_equal_byte(SetRes);  //usa el mismo código
  res.Invert;  //Invierte la lógica
end;
procedure TGenCod.Oper_byte_difer_bit(SetRes: boolean);
begin
  {No hay problema en usar siempre ExchangeP1_P2, porque el caso Expresión-Expresión,
  no se implementa Oper_bit_dif_byte.}
  ExchangeP1_P2;
  Oper_bit_dif_byte(SetRes);
end;
procedure TGenCod.Oper_byte_great_byte(SetRes: boolean);
var
  tmp: TPicRegister;
begin
  case catOperation of
  coConst_Const: begin  //compara constantes. Caso especial
    SetROPResultConst_bool(p1^.valInt > p2^.valInt);
  end;
  coConst_Variab: begin
    if p1^.valInt = 0 then begin
      //0 es mayor que nada
      SetROPResultConst_bool(false);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
      _MOVLW(p1^.valInt);
      _BANKSEL(p2^.bank);  //verifica banco destino
      _SUBWF(p2^.offs, toW);  //Si p1 > p2: C=0.
      CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
    end;
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    if p1^.valInt = 0 then begin
      //0 es mayor que nada
      SetROPResultConst_bool(false);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      //Optimiza rutina, usando: A>B  equiv. NOT (B<=A-1)
      //Se necesita asegurar que p1, es mayo que cero.
      SetROPResultExpres_bool(operType, true);  //invierte la lógica
      //p2, ya está en W
      _SUBLW(p1^.valInt-1);  //Si p1 > p2: C=0.
      CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
    end;
  end;
  coVariab_Const: begin
    if p2^.valInt = 255 then begin
      //Nada es mayor que 255
      SetROPResultConst_bool(false);
      GenWarn('Expression will always be FALSE or TRUE.');
    end else begin
      SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVF(p1^.offs, toW);
      _SUBLW(p2^.valInt);  //Si p1 > p2: C=0.
      CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
    end;
  end;
  coVariab_Variab:begin
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.offs, toW);
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.offs, toW);  //Si p1 > p2: C=0.
    CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
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
      SetROPResultConst_bool(false);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
  //    p1, ya está en W
      _SUBLW(p2^.valInt);  //Si p1 > p2: C=0.
      CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
    end;
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.offs, toW);  //Si p1 > p2: C=0.
    CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
  end;
  coExpres_Expres:begin
    //la expresión p1 debe estar salvada y p2 en el acumulador
    p1^.SetAsVariab(GetVarByteFromStk);  //Convierte a variable
    catOperation := TCatOperation((Ord(p1^.Cat) << 2) or ord(p2^.Cat));
    //Luego el caso es similar a coVariab_Expres
    Oper_byte_great_byte(true);
    FreeStkRegisterByte;   //libera pila porque ya se usó el dato ahí contenido
  end;
  end;
end;
procedure TGenCod.Oper_byte_less_byte(SetRes: boolean);
begin
  //A < B es lo mismo que B > A
  case catOperation of
  coExpres_Expres:begin
    {Este es el único caso que no se puede invertir, por la posición de los operandos en
     la pila.}
    //la expresión p1 debe estar salvada y p2 en el acumulador
    p1^.SetAsVariab(GetVarByteFromStk);  //Convierte a variable
    catOperation := TCatOperation((Ord(p1^.Cat) << 2) or ord(p2^.Cat));
    //Luego el caso es similar a coVariab_Expres
    Oper_byte_less_byte(SetRes);
    FreeStkRegisterByte;   //libera pila porque ya se usó el dato ahí contenido
  end;
  else
    //Para los otros casos, funciona
    ExchangeP1_P2;
    Oper_byte_great_byte(SetRes);
  end;
end;
procedure TGenCod.Oper_byte_gequ_byte(SetRes: boolean);
begin
  Oper_byte_less_byte(SetRes);
  res.Invert;
end;
procedure TGenCod.Oper_byte_lequ_byte(SetRes: boolean);
begin
  Oper_byte_great_byte(SetRes);
  res.Invert;
end;
procedure TGenCod.CodifShift_by_W(aux: TPicRegister; toRight: boolean);
{Desplaza el registro "aux", las veces indicadas en el registro W.
Deja el resultado en W.
Deja el banco, en el banco de "aux"}
{ TODO : Tal vez se pueda optimizar usando una rutina que rote W, las veces indicadas
en un registro, o se podría generar el código usando la rutina de WHILE. }
var
  loop1: Word;
  dg: integer;
begin
  _BANKSEL(aux.bank);  //quedará en este banco
  _ADDLW(1);   //corrige valor inicial
loop1 := _PC;
  _ADDLW(255);  //W=W-1  (no hay instrucción DECW)
  _BTFSC(Z.offs, Z.bit);
  _GOTO_PEND(dg);     //Dio, cero, termina
  //Desplaza
  _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
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
procedure TGenCod.Oper_byte_shr_byte(SetRes: boolean);  //Desplaza a la derecha
var
  aux: TPicRegister;
begin
  case catOperation of
  coConst_Const: begin  //compara constantes. Caso especial
    SetROPResultConst_byte(p1^.valInt >> p2^.valInt);
  end;
//  coConst_Variab: begin
//  end;
//  coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
//  end;
  coVariab_Const: begin
    SetROPResultExpres_byte(operType);   //Se pide Z para el resultado
    //Verifica casos simples
    if p2^.valInt = 0 then begin
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVF(p1^.offs, toW);  //solo devuelve lo mismo en W
    end else if p2^.valInt = 1 then begin
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _BANKSEL(p1^.bank);  //verifica banco destino
      _RRF(p1^.offs, toW);  //devuelve desplazado en W
    end else if p2^.valInt = 2 then begin
      aux := GetAuxRegisterByte;
      //copia p1 a "aux"
      _BANKSEL(p1^.bank);  //verifica banco destino
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(p1^.offs, toW);  //desplaza y mueve
      _BANKSEL(aux.bank);
      _MOVWF(aux.offs);
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toW);  //desplaza y devuelve en W
      aux.used := false;
    end else if p2^.valInt = 3 then begin
      aux := GetAuxRegisterByte;
      //copia p1 a "aux"
      _BANKSEL(p1^.bank);  //verifica banco destino
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(p1^.offs, toW);  //desplaza y mueve
      _BANKSEL(aux.bank);
      _MOVWF(aux.offs);
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toF);  //desplaza
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toW);  //desplaza y devuelve en W
      aux.used := false;
    end else if p2^.valInt = 4 then begin
      aux := GetAuxRegisterByte;
      //copia p1 a "aux"
      _BANKSEL(p1^.bank);  //verifica banco destino
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(p1^.offs, toW);  //desplaza y mueve
      _BANKSEL(aux.bank);
      _MOVWF(aux.offs);
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toF);  //desplaza
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toF);  //desplaza
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
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
    SetROPResultExpres_byte(operType);   //Se pide Z para el resultado
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
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetROPResultExpres_byte(operType);   //Se pide Z para el resultado
    //Verifica casos simples
    if p2^.valInt = 0 then begin
      //solo devuelve lo mismo en W
    end else if p2^.valInt = 1 then begin
      aux := GetAuxRegisterByte;
      _MOVWF(aux.offs);
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toW);  //devuelve desplazado en W
      aux.used := false;
    end else if p2^.valInt = 2 then begin
      aux := GetAuxRegisterByte;
      _MOVWF(aux.offs);   //copia p1 a "aux"
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toF);  //desplaza
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toW);  //desplaza y devuelve en W
      aux.used := false;
    end else if p2^.valInt = 3 then begin
      aux := GetAuxRegisterByte;
      _MOVWF(aux.offs);   //copia p1 a "aux"
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toF);  //desplaza
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toF);  //desplaza
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toW);  //desplaza y devuelve en W
      aux.used := false;
    end else if p2^.valInt = 4 then begin
      aux := GetAuxRegisterByte;
      _MOVWF(aux.offs);   //copia p1 a "aux"
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toF);  //desplaza
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toF);  //desplaza
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toF);  //desplaza
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toW);  //desplaza y devuelve en W
      aux.used := false;
    end else begin
      aux := GetAuxRegisterByte;
      //copia p1 a "aux"
      _MOVWF(aux.offs);
      //copia p2 a W
      _MOVLW(p2^.valInt);
      //lazo de rotación
      CodifShift_by_W(aux, true);
      aux.used := false;
    end;
  end;
//  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
//  end;
//  coExpres_Expres:begin
//  end;
  else
    genError('Not implemented: "%s"', [CatOperationToStr]);
  end;
end;
procedure TGenCod.Oper_byte_shl_byte(SetRes: boolean);   //Desplaza a la izquierda
var
  aux: TPicRegister;
begin
  case catOperation of
  coConst_Const: begin  //compara constantes. Caso especial
    SetROPResultConst_byte(p1^.valInt << p2^.valInt);
  end;
//  coConst_Variab: begin
//  end;
//  coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
//  end;
  coVariab_Const: begin
    SetROPResultExpres_byte(operType);   //Se pide Z para el resultado
    //Verifica casos simples
    if p2^.valInt = 0 then begin
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVF(p1^.offs, toW);  //solo devuelve lo mismo en W
    end else if p2^.valInt = 1 then begin
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _BANKSEL(p1^.bank);  //verifica banco destino
      _RLF(p1^.offs, toW);  //devuelve desplazado en W
    end else if p2^.valInt = 2 then begin
      aux := GetAuxRegisterByte;
      //copia p1 a "aux"
      _BANKSEL(p1^.bank);  //verifica banco destino
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(p1^.offs, toW);  //desplaza y mueve
      _BANKSEL(aux.bank);
      _MOVWF(aux.offs);
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toW);  //desplaza y devuelve en W
      aux.used := false;
    end else if p2^.valInt = 3 then begin
      aux := GetAuxRegisterByte;
      //copia p1 a "aux"
      _BANKSEL(p1^.bank);  //verifica banco destino
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(p1^.offs, toW);  //desplaza y mueve
      _BANKSEL(aux.bank);
      _MOVWF(aux.offs);
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toF);  //desplaza
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toW);  //desplaza y devuelve en W
      aux.used := false;
    end else if p2^.valInt = 4 then begin
      aux := GetAuxRegisterByte;
      //copia p1 a "aux"
      _BANKSEL(p1^.bank);  //verifica banco destino
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(p1^.offs, toW);  //desplaza y mueve
      _BANKSEL(aux.bank);
      _MOVWF(aux.offs);
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toF);  //desplaza
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toF);  //desplaza
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
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
    SetROPResultExpres_byte(operType);   //Se pide Z para el resultado
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
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetROPResultExpres_byte(operType);   //Se pide Z para el resultado
    //Verifica casos simples
    if p2^.valInt = 0 then begin
      //solo devuelve lo mismo en W
    end else if p2^.valInt = 1 then begin
      aux := GetAuxRegisterByte;
      _MOVWF(aux.offs);
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toW);  //devuelve desplazado en W
      aux.used := false;
    end else if p2^.valInt = 2 then begin
      aux := GetAuxRegisterByte;
      _MOVWF(aux.offs);   //copia p1 a "aux"
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toF);  //desplaza
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toW);  //desplaza y devuelve en W
      aux.used := false;
    end else if p2^.valInt = 3 then begin
      aux := GetAuxRegisterByte;
      _MOVWF(aux.offs);   //copia p1 a "aux"
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toF);  //desplaza
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toF);  //desplaza
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toW);  //desplaza y devuelve en W
      aux.used := false;
    end else if p2^.valInt = 4 then begin
      aux := GetAuxRegisterByte;
      _MOVWF(aux.offs);   //copia p1 a "aux"
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toF);  //desplaza
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toF);  //desplaza
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toF);  //desplaza
      _BCF(STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toW);  //desplaza y devuelve en W
      aux.used := false;
    end else begin
      aux := GetAuxRegisterByte;
      //copia p1 a "aux"
      _MOVWF(aux.offs);
      //copia p2 a W
      _MOVLW(p2^.valInt);
      //lazo de rotación
      CodifShift_by_W(aux, false);
      aux.used := false;
    end;
  end;
//  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
//  end;
//  coExpres_Expres:begin
//  end;
  else
    genError('Not implemented: "%s"', [CatOperationToStr]);
  end;
end;
//////////// Operaciones con Word
procedure TGenCod.Oper_word_asig_word(SetRes: boolean);
begin
  if p1^.Cat <> coVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.Cat of
  coConst : begin
    SetROPResultExpres_word(operType);  //Realmente, el resultado no es importante
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
    SetROPResultExpres_word(operType);  //Realmente, el resultado no es importante
    _BANKSEL(p2^.bank);
    _MOVF(p2^.Loffs, toW);
    _BANKSEL(p1^.bank);
    _MOVWF(p1^.Loffs);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.Hoffs, toW);
    _BANKSEL(p1^.bank);
    _MOVWF(p1^.Hoffs);
  end;
  coExpres: begin   //se asume que se tiene en (H,w)
    SetROPResultExpres_word(operType);  //Realmente, el resultado no es importante
    _BANKSEL(p1^.bank);
    _MOVWF(p1^.Loffs);
    _BANKSEL(H.bank);
    _MOVF(H.offs, toW);
    _BANKSEL(p1^.bank);
    _MOVWF(p1^.Hoffs);
  end;
  else
    GenError('No soportado'); exit;
  end;
end;
procedure TGenCod.Oper_word_asig_byte(SetRes: boolean);
begin
  if p1^.Cat <> coVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.Cat of
  coConst : begin
    SetROPResultExpres_word(operType);  //Realmente, el resultado no es importante
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
    SetROPResultExpres_word(operType);  //Realmente, el resultado no es importante
    _CLRF(p1^.Hoffs);
    _MOVF(p2^.Loffs, toW);
    _MOVWF(p1^.Loffs);
  end;
  coExpres: begin   //se asume que está en w
    SetROPResultExpres_word(operType);  //Realmente, el resultado no es importante
    _CLRF(p1^.Hoffs);
    _MOVWF(p1^.offs);
  end;
  else
    GenError('No soportado'); exit;
  end;
end;
procedure TGenCod.Oper_word_equal_word(SetRes: boolean);
var
  tmp: TPicRegister;
  sale: integer;
begin
  case catOperation of
  coConst_Const: begin  //compara constantes. Caso especial
    SetROPResultConst_bool(p1^.valInt = p2^.valInt);
  end;
  coConst_Variab: begin
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
    ////////// Compara byte alto
    if p1^.HByte = 0 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _MOVF(p2^.Hoffs, toW); //p2-p1
      _BTFSS(Z.offs, Z.bit);
      _GOTO_PEND(sale);  //no son iguales
    end else if p1^.HByte = 1 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _DECF(p2^.Hoffs, toW); //p2-p1
      _BTFSS(Z.offs, Z.bit);
      {De no ser porque se tiene que devolver siempre, el valor de Z,
      las 2 instrucciones anteriores, se podrían reemplazar con un DECFSZ,
      pero DECFSZ, no actualiza Z}
      _GOTO_PEND(sale);  //no son iguales
    end else if p1^.HByte = 255 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _INCF(p2^.Hoffs, toW); //p2-p1
      _BTFSS(Z.offs, Z.bit);
      {De no ser porque se tiene que devolver siempre, el valor de Z,
      las 2 instrucciones anteriores, se podrían reemplazar con un DECFSZ,
      pero DECFSZ, no actualiza Z}
      _GOTO_PEND(sale);  //no son iguales
    end else begin  //caso general
      _MOVLW(p1^.HByte);
      _BANKSEL(p2^.bank);  //verifica banco destino
      _SUBWF(p2^.Hoffs, toW); //p2-p1
      _BTFSS(Z.offs, Z.bit);
      _GOTO_PEND(sale);  //no son iguales
    end;
    //////////  Son iguales, comparar el byte bajo
    if p1^.LByte = 0 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _MOVF(p2^.Loffs,toW);	//p2-p1
  _LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
    end else if p1^.LByte = 1 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _DECF(p2^.Loffs,toW);	//p2-p1
  _LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
    end else if p1^.LByte = 255 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _INCF(p2^.Loffs,toW);	//p2-p1
  _LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
    end else begin
      _MOVLW(p1^.LByte);
      _BANKSEL(p2^.bank);  //verifica banco destino
      _SUBWF(p2^.Loffs,toW);	//p2-p1
  _LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
    end;
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó p2 esta en W
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
    tmp := GetAuxRegisterByte;
    if HayError then exit;
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
    tmp.used := false;
  end;
  coVariab_Const: begin
    ExchangeP1_P2;  //Convierte a coConst_Variab
    Oper_word_equal_word(SetRes);
  end;
  coVariab_Variab:begin
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
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
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
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
    Oper_word_equal_word(SetRes);
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    ExchangeP1_P2;  //Convierte a coVariab_Expres;
    Oper_word_equal_word(SetRes);
  end;
  coExpres_Expres:begin
    //La expresión p1, debe estar salvada y p2 en (H,W)
    p1^.SetAsVariab(GetVarWordFromStk);
    catOperation := TCatOperation((Ord(p1^.Cat) << 2) or ord(p2^.Cat));
    //Luego el caso es similar a variable-expresión
    Oper_word_equal_word(SetRes);
    FreeStkRegisterWord;
  end;
  else
    genError('Not implemented: "%s"', [CatOperationToStr]);
  end;
end;
procedure TGenCod.Oper_word_difer_word(SetRes: boolean);
begin
  Oper_word_equal_word(SetRes);
  res.Invert;
end;
procedure TGenCod.Oper_word_great_word(SetRes: boolean);
  procedure codVariab_Const;
  {Codifica el caso variable (p1) - constante (p2)}
  var
    sale: integer;
  begin
    if p2^.valInt = $FFFF then begin
      //Nada es mayor que $FFFF
      SetROPResultConst_bool(false);
      GenWarn('Expression will always be FALSE or TRUE.');
    end else begin
      //Compara byte alto
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVF(p1^.Hoffs, toW);
      _SUBLW(p2^.HByte); //p2-p1
      _BTFSS(Z.offs, Z.bit);
      _GOTO_PEND(sale);  //no son iguales
      //Son iguales, comparar el byte bajo
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVF(p1^.Loffs, toW);
      _SUBLW(p2^.LByte);	//p2-p1
  _LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
      CopyInvert_C_to_Z;  //Pasa a Z
    end;
  end;
  procedure codVariab_Variab;
  var
    sale: integer;
  begin
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
var
  tmp, aux: TPicRegister;
  sale: integer;
  varTmp: TxpEleVar;
begin
  case catOperation of
  coConst_Const: begin  //compara constantes. Caso especial
    SetROPResultConst_bool(p1^.valInt > p2^.valInt);
  end;
  coConst_Variab: begin
    if p1^.valInt = 0 then begin
      //0 es mayor que nada
      SetROPResultConst_bool(false);
      GenWarn('Expression will always be FALSE or TRUE.');
      {No se define realmente el mensaje (si es con TRUE o FALSE), porque
      Oper_word_great_word(), es también llamado, por Oper_word_lequ_word para con
      lógica invertida}
    end else begin
      SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
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
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
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
    tmp.used := false;
  end;
  coVariab_Const: begin
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
    codVariab_Const;
  end;
  coVariab_Variab:begin
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
    codVariab_Variab;
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en H,W
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
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
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en H,W
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
    aux := GetAuxRegisterByte;  //Pide un registro libre
    _MOVWF(aux.offs);  //guarda W
    varTmp := NewTmpVarWord(aux, H);  //Crea variable temporal
    p1^.SetAsVariab(varTmp);  //para que se pueda procesar como variable
    codVariab_Const;      //Lo evalúa como coVariab_Const
    varTmp.Destroy;
    aux.used := false;
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en H,W
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
    aux := GetAuxRegisterByte;  //Pide un registro libre
    _MOVWF(aux.offs);  //guarda W
    varTmp := NewTmpVarWord(aux, H);  //Crea variable temporal
    p1^.SetAsVariab(varTmp);  //para que se pueda procesar como variable
    codVariab_Variab;      //Lo evalúa como coVariab_Variab;
    varTmp.Destroy;
    aux.used := false;
  end;
  coExpres_Expres:begin
    //La expresión p1, debe estar salvada y p2 en (H,W)
    p1^.SetAsVariab(GetVarWordFromStk);
    catOperation := TCatOperation((Ord(p1^.Cat) << 2) or ord(p2^.Cat));
    //Luego el caso es similar a variable-expresión
    Oper_word_great_word(SetRes);
    FreeStkRegisterWord;
  end;
  else
    genError('Not implemented: "%s"', [CatOperationToStr]);
  end;
end;
procedure TGenCod.Oper_word_add_word(SetRes: boolean);
var
  aux: TPicRegister;
begin
  case catOperation of
  coConst_Const: begin
    if p1^.valInt+p2^.valInt <256 then begin
      //Optimiza
      SetROPResultConst_byte(p1^.valInt+p2^.valInt);
    end else begin
      SetROPResultConst_word(p1^.valInt+p2^.valInt);
    end;
  end;
  coConst_Variab: begin
    SetROPResultExpres_word(operType);
{     aux := GetUnusedByteRegister;  //Pide un registro libre
    _movlw(p1^.LByte);      //Carga menos peso del dato 1
    _addwf(p2^.Loffs,toW);  //Suma menos peso del dato 2
    _movwf(aux);             //Almacena el resultado
    _movlw(p1^.HByte);      //Carga más peso del dato 1
    _btfsc(STATUS,_C);    //Hubo acarreo anterior?
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
    _btfsc(STATUS,_C);     //Hubo acarreo anterior?
    _incf(H.offs, toF);
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y esta en (H,W)
    SetROPResultExpres_word(operType);
    aux := GetAuxRegisterByte;  //Pide un registro libre
    _movwf(aux.offs);             //guarda byte bajo
    _movlw(p1^.HByte);      //Carga más peso del dato 1
    _addwf(H.offs,toF);         //Suma y guarda
    _movlw(p1^.LByte);      //Carga menos peso del dato 1
    _addwf(aux.offs,toW);         //Suma menos peso del dato 2, deja en W
    _btfsc(STATUS,_C);    //Hubo acarreo anterior?
    _incf(H.offs, toF);
    aux.used := false;
  end;
  coVariab_Const: begin
    SetROPResultExpres_word(operType);
    _MOVLW(p2^.HByte);      //Carga más peso del dato 1
    _ADDWF(p1^.Hoffs,toW);  //Suma más peso del dato 2
    _MOVWF(H.offs);         //Guarda el resultado
    _MOVLW(p2^.LByte);      //Carga menos peso del dato 1
    _ADDWF(p1^.Loffs,toW);  //Suma menos peso del dato 2, deja en W
    _BTFSC(STATUS,_C);     //Hubo acarreo anterior?
    _INCF(H.offs, toF);
  end;
  coVariab_Variab:begin
    SetROPResultExpres_word(operType);
    _MOVF(p1^.Hoffs, toW);  //Carga mayor peso del dato 1
    _ADDWF(p2^.Hoffs,toW);  //Suma mayor peso del dato 2
    _MOVWF(H.offs);         //Guarda mayor peso del resultado
    _MOVF(p1^.Loffs, toW);  //Carga menos peso del dato 1
    _ADDWF(p2^.Loffs,toW);  //Suma menos peso del dato 2, deja en W
    _BTFSC(STATUS,_C);     //Hubo acarreo anterior?
    _INCF(H.offs, toF);
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en (H,W)
    SetROPResultExpres_word(operType);
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
    _btfsc(STATUS,_C);      //Hubo acarreo anterior?
    _incf(H.offs, toF);
    aux.used := false;
  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,W)
    SetROPResultExpres_word(operType);
    aux := GetAuxRegisterByte;  //Pide un registro libre
    _movwf(aux.offs);             //guarda byte bajo
    _movlw(p2^.HByte);      //Carga más peso del dato 1
    _addwf(H.offs,toF);         //Suma y guarda
    _movlw(p2^.LByte);      //Carga menos peso del dato 1
    _addwf(aux.offs,toW);         //Suma menos peso del dato 2, deja en W
    _btfsc(STATUS,_C);    //Hubo acarreo anterior?
    _incf(H.offs, toF);
    aux.used := false;
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,W)
    SetROPResultExpres_word(operType);
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
    _btfsc(STATUS,_C);    //Hubo acarreo anterior?
    _incf(H.offs, toF);
    aux.used := false;
  end;
  coExpres_Expres:begin
    SetROPResultExpres_word(operType);
    //p1 está salvado en pila y p2 en (_H,W)
    p1^.SetAsVariab(GetVarWordFromStk);  //Convierte a variable
    catOperation := TCatOperation((Ord(p1^.Cat) << 2) or ord(p2^.Cat));
    //Luego el caso es similar a coVariab_Expres
    Oper_word_add_word(SetRes);
    FreeStkRegisterWord;   //libera pila, obtiene dirección
  end;
  else
    genError('Not implemented: "%s"', [CatOperationToStr]);
  end;
end;
procedure TGenCod.Oper_word_add_byte(SetRes: boolean);
var
  aux: TPicRegister;
begin
  case catOperation of
  coConst_Const: begin
    if p1^.valInt+p2^.valInt <256 then begin
      //Optimiza
      SetROPResultConst_byte(p1^.valInt+p2^.valInt);
    end else begin
      SetROPResultConst_word(p1^.valInt+p2^.valInt);
    end;
  end;
  coConst_Variab: begin
    SetROPResultExpres_word(operType);
    //versión más corta que solo usa _H, por validar
    _movlw(p1^.HByte);      //Carga más peso del dato 1
    _BANKSEL(H.bank);
    _movwf(H.offs);
    _movlw(p1^.LByte);      //Carga menos peso del dato 1
    _BANKSEL(p2^.bank);
    _addwf(p2^.Loffs,toW);  //Suma menos peso del dato 2, deja en W
    _btfsc(STATUS,_C);    //Hubo acarreo anterior?
    _incf(H.offs, toF);
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y esta en (W)
    SetROPResultExpres_word(operType);
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
    _btfsc(STATUS,_C);    //Hubo acarreo anterior?
    _incf(H.offs, toF);
    aux.used := false;
  end;
  coVariab_Const: begin
    SetROPResultExpres_word(operType);
    _BANKSEL(p1^.bank);      //se cambia primero el banco por si acaso
    _MOVF(p1^.Hoffs, toW); //Carga más peso del dato 1
    _BANKSEL(H.bank);      //se cambia primero el banco por si acaso
    _MOVWF(H.offs);        //Guarda el resultado
    _MOVLW(p2^.LByte);
    _BANKSEL(p1^.bank);      //se cambia primero el banco por si acaso
    _ADDWF(p1^.Loffs,toW); //Suma menos peso del dato 2, deja en W
    _BANKSEL(H.bank);      //se cambia primero el banco, por si acaso.
    _BTFSC(STATUS,_C);    //Hubo acarreo anterior?
    _INCF(H.offs, toF);
  end;
  coVariab_Variab:begin
    SetROPResultExpres_word(operType);
    _BANKSEL(p1^.bank);
    _MOVF(p1^.Hoffs, toW);     //Carga más peso del dato 1
    _BANKSEL(H.bank);
    _MOVWF(H.offs);
    _BANKSEL(p1^.bank);
    _MOVF(p1^.Loffs, toW);     //Carga menos peso del dato 1
    _BANKSEL(p2^.bank);
    _ADDWF(p2^.Loffs,toW); //Suma menos peso del dato 2, deja en W
    _BANKSEL(H.bank);      //se cambia primero el banco, por si acaso.
    _BTFSC(STATUS,_C);    //Hubo acarreo anterior?
    _INCF(H.offs, toF);
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en (_H,W)
    SetROPResultExpres_word(operType);
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
    _btfsc(STATUS,_C);    //Hubo acarreo anterior?
    _incf(H.offs, toF);
    aux.used := false;
  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,W)
    SetROPResultExpres_word(operType);
    _addlw(p2^.LByte);     //Suma menos peso del dato 2, deja en W
    _BANKSEL(H.bank);      //se cambia primero el banco, por si acaso.
    _btfsc(STATUS,_C);    //Hubo acarreo anterior?
    _incf(H.offs, toF);
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,W)
    SetROPResultExpres_word(operType);
    _BANKSEL(p2^.bank);
    _addwf(p2^.Loffs,toW);         //Suma menos peso del dato 2, deja en W
    _BANKSEL(H.bank);      //se cambia primero el banco, por si acaso.
    _btfsc(STATUS,_C);    //Hubo acarreo anterior?
    _incf(H.offs, toF);
  end;
  coExpres_Expres:begin
    SetROPResultExpres_word(operType);
    //p1 está salvado en pila y p2 en (_H,W)
    p1^.SetAsVariab(GetVarWordFromStk);  //Convierte a variable
    catOperation := TCatOperation((Ord(p1^.Cat) << 2) or ord(p2^.Cat));
    //Luego el caso es similar a coVariab_Expres
    Oper_word_add_byte(SetRes);
    FreeStkRegisterWord;   //libera pila
  end;
  else
    genError('Not implemented: "%s"', [CatOperationToStr] );
  end;
end;
procedure TGenCod.Oper_word_sub_word(SetRes: boolean);
var
  aux: TPicRegister;
begin
  case catOperation of
  coConst_Const: begin
    if p1^.valInt-p2^.valInt < 0 then begin
      genError('Numeric value exceeds a word range.', [CatOperationToStr]);
      exit;
    end;
    if p1^.valInt-p2^.valInt <256 then begin
      //Optimiza
      SetROPResultConst_byte(p1^.valInt-p2^.valInt);
    end else begin
      SetROPResultConst_word(p1^.valInt-p2^.valInt);
    end;
  end;
  coConst_Variab: begin
    SetROPResultExpres_word(operType);
    _movf (p2^.Hoffs,toW);  //p2->w
    _SUBLW(p1^.HByte);     //p1 - W -W
    _movwf(H.offs);
    _movf (p2^.Loffs,toW);  //p2-W
    _SUBLW(p1^.LByte);      //p1-W->w
    _btfss(STATUS, _C);
    _decf(H.offs,toF);
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y esta en (H,W)
    SetROPResultExpres_word(operType);
    aux := GetAuxRegisterByte;
    _MOVWF(aux.offs);
    _movf (H.offs,toW);    //p2->w
    _SUBLW(p1^.HByte);     //p1 - W -W
    _movwf(H.offs);
    _movf (aux.offs,toW);  //p2-W
    _SUBLW(p1^.LByte);     //p1-W->w
    _btfss(STATUS, _C);
    _decf(H.offs,toF);
    aux.used := false;
  end;
  coVariab_Const: begin
    SetROPResultExpres_word(operType);
    _movlw(p2^.HByte);
    _subwf(p1^.Hoffs,toW);
    _movwf(H.offs);
    _movlw(p2^.LByte);
    _subwf(p1^.Loffs,toW);
    _btfss(STATUS, _C);
    _decf(H.offs,toF);
  end;
  coVariab_Variab:begin  //p1 - p2
    SetROPResultExpres_word(operType);
    _movf (p2^.Hoffs,toW);
    _subwf(p1^.Hoffs,toW);
    _movwf(H.offs);
    _movf (p2^.Loffs,toW);
    _subwf(p1^.Loffs,toW);
    _btfss(STATUS, _C);
    _decf(H.offs,toF);
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en (H,W)
    SetROPResultExpres_word(operType);
    aux := GetAuxRegisterByte;  //Pide un registro libre
    _MOVWF(aux.offs);
    _movf (H.offs,toW);
    _subwf(p1^.Hoffs,toW);
    _movwf(H.offs);
    _movf (aux.offs,toW);
    _subwf(p1^.Loffs,toW);
    _btfss(STATUS, _C);
    _decf(H.offs,toF);
    aux.used := false;
  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,W)
    SetROPResultExpres_word(operType);
    aux := GetAuxRegisterByte;  //Pide un registro libre
    _MOVWF(aux.offs);
    _movlw(p2^.HByte);
    _subwf(H.offs, toF);
    _movlw(p2^.LByte);
    _subwf(aux.offs,toW);
    _btfss(STATUS, _C);
    _decf(H.offs,toF);
    aux.used := false;
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,W)
    SetROPResultExpres_word(operType);
    aux := GetAuxRegisterByte;  //Pide un registro libre
    _MOVWF(aux.offs);
    _movf(p2^.Hoffs, toW);
    _subwf(H.offs, toF);
    _movf(p2^.Loffs, toW);
    _subwf(aux.offs,toW);
    _btfss(STATUS, _C);
    _decf(H.offs,toF);
    aux.used := false;
  end;
  coExpres_Expres:begin
    SetROPResultExpres_word(operType);
    //p1 está salvado en pila y p2 en (_H,W)
    p1^.SetAsVariab(GetVarWordFromStk);  //Convierte a variable
    catOperation := TCatOperation((Ord(p1^.Cat) << 2) or ord(p2^.Cat));
    //Luego el caso es similar a coVariab_Expres
    Oper_word_sub_word(SetRes);
    FreeStkRegisterWord;   //libera pila, obtiene dirección
  end;
  else
    genError('Not implemented: "%s"', [CatOperationToStr]);
  end;
end;
procedure TGenCod.word_mul_word_16(fun: TxpEleFun);
var
  SYSTMP00, SYSTMP01, SYSTMP02: TPicRegister;
  MUL16LOOP: Word;
begin
   //[H_W] = [H_W] x [E_U]
   // RES  =  OP_A x OP_B
   //SYSTMP00 variable temporal. Contiene RES.LOW (resultado.LOW de la multiplicación)
   //SYSTMP01 variable temporal. Contiene OP_A.LOW  (inicialmente W)
   //SYSTMP02 variable temporal. Contiene OP_A.HIGH (inicialmente _H)
   //_H contine durante todo el bucle de multiplicación la parte alta de resultado (RES.HIGH)
   StartCodeSub(fun);  //inicia codificación
   typWord.DefineRegister;   //Asegura que exista H,W.
   SYSTMP00 := GetAuxRegisterByte;  //Pide un registro libre
   SYSTMP01  := GetAuxRegisterByte;  //Pide un registro libre
   SYSTMP02  := GetAuxRegisterByte;  //Pide un registro libre
   if HayError then exit;
   _CLRF    (SYSTMP00.offs);    //Clear RES.LOW
   _MOVWF   (SYSTMP01.offs);    //OP_A.LOW  := W
   _MOVF    (H.offs,toW    );    //OP_A.HIGH := H.offs
   _MOVWF   (SYSTMP02.offs);
   _CLRF    (H.offs);          //Clear RES.HIGH
MUL16LOOP := _PC;
   _BTFSS   (U.offs,0);   //Si (OP_B.0=1) then RES+=OP_A
   _GOTO    (_PC+7);      //END_IF_1
   _MOVF    (SYSTMP01.offs,toW);
   _ADDWF   (SYSTMP00.offs,toF);
   _MOVF    (SYSTMP02.offs,toW);
   _BTFSC   (STATUS,0  );
   _ADDLW   (1);
   _ADDWF   (H.offs,toF);
// END_IF_1:
   _BCF     (STATUS, 0);    //STATUS.C := 0
   _RRF     (E.offs, toF    );    //OP_B>>1
   _RRF     (U.offs, toF    );
   _BCF     (STATUS, 0);    //STATUS.C := 0
   _RLF     (SYSTMP01.offs,toF);  //OP_A<<1
   _RLF     (SYSTMP02.offs,toF);
   _MOVF    (E.offs, toW);  //Si (OP_B>0) then goto MUL16LOOP
   _IORWF   (U.offs, toW);
   _BTFSS   (STATUS, 2);
   _GOTO    (MUL16LOOP);  //OP_B>0
   _MOVF    (SYSTMP00.offs, toW);  //Return RES.LOW to toW
   SYSTMP00.used := false;
   SYSTMP01.used := false;
   SYSTMP02.used := false;
   EndCodeSub;  //termina codificación
end;
procedure TGenCod.Oper_word_umulword_word(SetRes: boolean);
begin
  case catOperation of
  coConst_Const:begin  //producto de dos constantes. Caso especial
    SetROPResultConst_word((p1^.valInt*p2^.valInt) and $FFFF);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
//  coConst_Variab: begin
//    SetROPResultExpres_word(operType);
//    _BANKSEL(p2^.bank);
//    _MOVF(p2^.offs, toW);
//    _BANKSEL(H.bank);
//    _MOVWF(H.offs);
//    _MOVLW(p1^.valInt);
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
//    _BANKSEL(H.bank);
//    _MOVWF(H.offs);
//    _MOVLW(p1^.valInt);
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  coVariab_Const: begin
//    SetROPResultExpres_byte(operType);
//    _BANKSEL(p1^.bank);
//    _MOVF(p1^.offs, toW);
//    _BANKSEL(H.bank);
//    _MOVWF(H.offs);
//    _MOVLW(p2^.valInt);
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  coVariab_Variab:begin
//    SetROPResultExpres_byte(operType);
//    _BANKSEL(p1^.bank);
//    _MOVF(p1^.offs, toW);
//    _BANKSEL(H.bank);
//    _MOVWF(H.offs);
//    _BANKSEL(p2^.bank);
//    _MOVF(p2^.offs, toW);
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
//    _BANKSEL(H.bank);
//    _MOVWF(H.offs);  //p2 -> H
//    _BANKSEL(p1^.bank);
//    _MOVF(p1^.offs, toW); //p1 -> W
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
//    _MOVWF(H.offs);  //p1 -> H
//    _MOVLW(p2^.valInt); //p2 -> W
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
//    _BANKSEL(H.bank);
//    _MOVWF(H.offs);  //p1 -> H
//    _BANKSEL(p2^.bank);
//    _MOVF(p2^.offs, toW); //p2 -> W
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  coExpres_Expres:begin
//    SetROPResultExpres_byte(operType);
//    //la expresión p1 debe estar salvada y p2 en el acumulador
//    FreeStkRegisterByte(r);   //libera pila porque se usará el dato ahí contenido
//    _BANKSEL(H.bank);
//    _MOVWF(H.offs);  //p2 -> H
//    _BANKSEL(r.bank);
//    _MOVF(r.offs, toW); //p1 -> W
//    _CALL(f_byteXbyte_byte.adrr);
//    {Se podría ahorrar el paso de mover la variable de la pila a W (y luego a una
//    variable) temporal, si se tuviera una rutina de multiplicación que compilara a
//    partir de la direccion de una variable (en este caso de la pila, que se puede
//    modificar), pero es un caso puntual, y podría no reutilizar el código apropiadamente.}
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
  else
    genError('Not implemented: "%s"', [CatOperationToStr]);
  end;
end;
procedure TGenCod.Oper_word_and_byte(SetRes: boolean);
begin
  case catOperation of
  coConst_Const: begin
    //Optimiza
    SetROPResultConst_byte(p1^.valInt and p2^.valInt);
  end;
  coConst_Variab: begin
    SetROPResultExpres_byte(operType);
    _movlw(p1^.LByte);      //Carga menos peso del dato 1
    _BANKSEL(p2^.bank);
    _andwf(p2^.Loffs,toW);  //deja en W
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y esta en (W)
    SetROPResultExpres_byte(operType);
    _andlw(p1^.LByte);      //Deja en W
  end;
  coVariab_Const: begin
    SetROPResultExpres_byte(operType);
    _BANKSEL(p1^.bank);
    _MOVF(p1^.Loffs, toW);
    _ANDLW(p2^.LByte);
  end;
  coVariab_Variab:begin
    SetROPResultExpres_byte(operType);
    _BANKSEL(p1^.bank);
    _MOVF(p1^.Loffs, toW);
    _BANKSEL(p2^.bank);
    _ANDWF(p2^.Loffs, toW);
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en (_H,W)
    SetROPResultExpres_byte(operType);
    _BANKSEL(p1^.bank);
    _ANDWF(p1^.Loffs, toW);
  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,W)
    SetROPResultExpres_byte(operType);
    _ANDLW(p2^.LByte);
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,W)
    SetROPResultExpres_byte(operType);
    _BANKSEL(p2^.bank);
    _ANDWF(p2^.Loffs, toW);
  end;
  coExpres_Expres:begin
    SetROPResultExpres_byte(operType);
    //p1 está salvado en pila y p2 en (W)
    p1^.SetAsVariab(GetVarWordFromStk);  //Convierte a variable
    //Luego el caso es similar a coVariab_Expres
    _BANKSEL(p1^.bank);
    _ANDWF(p1^.Loffs, toW);
    FreeStkRegisterWord;   //libera pila
  end;
  else
    genError('Not implemented: "%s"', [CatOperationToStr] );
  end;
end;

//////////// Operaciones con Dword
procedure TGenCod.Oper_dword_asig_byte(SetRes: boolean);
begin
  if p1^.Cat <> coVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.Cat of
  coConst : begin
    SetROPResultExpres_dword(operType);  //Realmente, el resultado no es importante
    if p2^.valInt = 0 then begin
      //caso especial
      _CLRF(p1^.Loffs);
      _CLRF(p1^.Hoffs);
      _CLRF(p1^.Eoffs);
      _CLRF(p1^.Uoffs);
    end else begin;
      _CLRF(p1^.Uoffs);
      _CLRF(p1^.Eoffs);
      _CLRF(p1^.Hoffs);
      _MOVLW(p2^.valInt);
      _MOVWF(p1^.Loffs);
    end;
  end;
  coVariab: begin
    SetROPResultExpres_dword(operType);  //Realmente, el resultado no es importante
    _CLRF(p1^.Uoffs);
    _CLRF(p1^.Eoffs);
    _CLRF(p1^.Hoffs);
    _MOVF(p2^.Loffs, toW);
    _MOVWF(p1^.Loffs);
  end;
  coExpres: begin   //se asume que está en w
    SetROPResultExpres_dword(operType);  //Realmente, el resultado no es importante
    _CLRF(p1^.Uoffs);
    _CLRF(p1^.Eoffs);
    _CLRF(p1^.Hoffs);
    _MOVWF(p1^.offs);
  end;
  else
    GenError('No soportado'); exit;
  end;
end;
procedure TGenCod.Oper_dword_asig_word(SetRes: boolean);
begin
  if p1^.Cat <> coVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.Cat of
  coConst : begin
    SetROPResultExpres_dword(operType);  //Realmente, el resultado no es importante
    if p2^.valInt = 0 then begin
      //caso especial
      _CLRF(p1^.Uoffs);
      _CLRF(p1^.Eoffs);
      _CLRF(p1^.Hoffs);
      _CLRF(p1^.Loffs);
    end else begin;
      _CLRF(p1^.Uoffs);
      _CLRF(p1^.Eoffs);
      _MOVLW(p2^.HByte);
      _MOVWF(p1^.Hoffs);
      _MOVLW(p2^.LByte);
      _MOVWF(p1^.Loffs);
    end;
  end;
  coVariab: begin
    SetROPResultExpres_dword(operType);  //Realmente, el resultado no es importante
    _CLRF(p1^.Uoffs);
    _CLRF(p1^.Eoffs);
    _MOVF(p2^.Hoffs, toW);
    _MOVWF(p1^.Hoffs);
    _MOVF(p2^.Loffs, toW);
    _MOVWF(p1^.Loffs);
  end;
  coExpres: begin   //se asume que está en w
    SetROPResultExpres_dword(operType);  //Realmente, el resultado no es importante
    _CLRF(p1^.Uoffs);
    _CLRF(p1^.Eoffs);
    _MOVWF(p1^.Loffs);
    _MOVF(H.offs, toW);
    _MOVWF(p1^.Hoffs);
  end;
  else
    GenError('No soportado'); exit;
  end;
end;
procedure TGenCod.Oper_dword_asig_dword(SetRes: boolean);
begin
  if p1^.Cat <> coVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.Cat of
  coConst : begin
    SetROPResultExpres_dword(operType);  //Realmente, el resultado no es importante
    if p2^.valInt = 0 then begin
      //caso especial
      _BANKSEL(p1^.bank);
      _CLRF(p1^.Uoffs);
      _CLRF(p1^.Eoffs);
      _CLRF(p1^.Hoffs);
      _CLRF(p1^.Loffs);
    end else begin;
      _BANKSEL(p1^.bank);
      _MOVLW(p2^.UByte);
      _MOVWF(p1^.Uoffs);
      _MOVLW(p2^.EByte);
      _MOVWF(p1^.Eoffs);
      _MOVLW(p2^.HByte);
      _MOVWF(p1^.Hoffs);
      _MOVLW(p2^.LByte);
      _MOVWF(p1^.Loffs);
    end;
  end;
  coVariab: begin
    SetROPResultExpres_dword(operType);  //Realmente, el resultado no es importante
    _BANKSEL(p2^.bank);
    _MOVF(p2^.Uoffs, toW);
    _BANKSEL(p1^.bank);
    _MOVWF(p1^.Uoffs);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.Eoffs, toW);
    _BANKSEL(p1^.bank);
    _MOVWF(p1^.Eoffs);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.Hoffs, toW);
    _BANKSEL(p1^.bank);
    _MOVWF(p1^.Hoffs);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.Loffs, toW);
    _BANKSEL(p1^.bank);
    _MOVWF(p1^.Loffs);
  end;
  coExpres: begin   //se asume que está en w
    SetROPResultExpres_dword(operType);  //Realmente, el resultado no es importante
    _MOVWF(p1^.Loffs);
    _BANKSEL(H.bank);
    _MOVF(H.offs, toW);
    _BANKSEL(p1^.bank);
    _MOVWF(p1^.Hoffs);
    _BANKSEL(E.bank);
    _MOVF(E.offs, toW);
    _BANKSEL(p1^.bank);
    _MOVWF(p1^.Eoffs);
    _BANKSEL(U.bank);
    _MOVF(U.offs, toW);
    _MOVWF(p1^.Uoffs);
  end;
  else
    GenError('No soportado'); exit;
  end;
end;
procedure TGenCod.Oper_dword_equal_dword(SetRes: boolean);
var
  sale1, sale2, sale3: integer;
begin
  case catOperation of
  coConst_Const: begin  //compara constantes. Caso especial
    SetROPResultConst_bool(p1^.valInt = p2^.valInt);
  end;
  coConst_Variab: begin
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
    //Compara byte U
    if p1^.UByte = 0 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _MOVF(p2^.Uoffs, toW); //p2=0?
      _BTFSS(Z.offs, Z.bit);
      _GOTO_PEND(sale1);  //no son iguales
    end else if p1^.UByte = 1 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _DECF(p2^.Uoffs, toW); //p2=1?
      _BTFSS(Z.offs, Z.bit);
      _GOTO_PEND(sale1);  //no son iguales
    end else if p1^.UByte = 255 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _INCF(p2^.Uoffs, toW); //p2=255?
      _BTFSS(Z.offs, Z.bit);
      _GOTO_PEND(sale1);  //no son iguales
    end else begin  //caso general
      _MOVLW(p1^.UByte);
      _BANKSEL(p2^.bank);  //verifica banco destino
      _SUBWF(p2^.Uoffs, toW); //p2-p1
      _BTFSS(Z.offs, Z.bit);
      _GOTO_PEND(sale1);  //no son iguales
    end;
    //Compara byte E
    if p1^.EByte = 0 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _MOVF(p2^.Eoffs, toW); //p2=0?
      _BTFSS(Z.offs, Z.bit);
      _GOTO_PEND(sale2);  //no son iguales
    end else if p1^.EByte = 1 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _DECF(p2^.Eoffs, toW); //p2=1?
      _BTFSS(Z.offs, Z.bit);
      _GOTO_PEND(sale2);  //no son iguales
    end else if p1^.EByte = 255 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _INCF(p2^.Eoffs, toW); //p2=255?
      _BTFSS(Z.offs, Z.bit);
      _GOTO_PEND(sale2);  //no son iguales
    end else begin  //caso general
      _MOVLW(p1^.EByte);
      _BANKSEL(p2^.bank);  //verifica banco destino
      _SUBWF(p2^.Eoffs, toW); //p2-p1
      _BTFSS(Z.offs, Z.bit);
      _GOTO_PEND(sale2);  //no son iguales
    end;
    //Compara byte H
    if p1^.HByte = 0 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _MOVF(p2^.Hoffs, toW); //p2=0?
      _BTFSS(Z.offs, Z.bit);
      _GOTO_PEND(sale3);  //no son iguales
    end else if p1^.HByte = 1 then begin
      _BANKSEL(p2^.bank);  //verifica banco destino
      _DECF(p2^.Hoffs, toW); //p2=1?
      _BTFSS(Z.offs, Z.bit);
      _GOTO_PEND(sale3);  //no son iguales
    end else if p1^.HByte = 255 then begin
      _BANKSEL(p2^.bank);  //verifica banco destino
      _INCF(p2^.Hoffs, toW); //p2=255?
      _BTFSS(Z.offs, Z.bit);
      _GOTO_PEND(sale3);  //no son iguales
    end else begin  //caso general
      _MOVLW(p1^.HByte);
      _BANKSEL(p2^.bank);  //verifica banco destino
      _SUBWF(p2^.Hoffs, toW); //p2-p1
      _BTFSS(Z.offs, Z.bit);
      _GOTO_PEND(sale3);  //no son iguales
    end;
    //Son iguales, comparar el byte bajo
    if p1^.LByte = 0 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _MOVF(p2^.Loffs,toW);	//p2=0?
    end else if p1^.LByte = 1 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _DECF(p2^.Loffs,toW);	//p2=1?
    end else if p1^.LByte = 255 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _INCF(p2^.Loffs,toW);	//p2=255?
    end else begin  //caso general
      _MOVLW(p1^.LByte);
      _BANKSEL(p2^.bank);  //verifica banco destino
      _SUBWF(p2^.Loffs,toW);	//p2-p1
    end;
_LABEL(sale1); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
_LABEL(sale2);
_LABEL(sale3);
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y está en UEHW
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
    //Compara byte L
    _SUBLW(p1^.LByte); //p2^.L está en W
    _BTFSS(Z.offs, Z.bit);
    _GOTO_PEND(sale1);  //no son iguales
    //Compara byte H
    _MOVLW(p1^.HByte);
    _SUBWF(H.offs, toW); //p2-p1
    _BTFSS(Z.offs, Z.bit);
    _GOTO_PEND(sale2);  //no son iguales
    //Compara byte E
    _MOVLW(p1^.EByte);
    _SUBWF(E.offs, toW); //p2-p1
    _BTFSS(Z.offs, Z.bit);
    _GOTO_PEND(sale3);  //no son iguales
    //Comparar el byte U
    _MOVLW(p1^.UByte);
    _SUBWF(U.offs,toW);	//p2-p1
_LABEL(sale1); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
_LABEL(sale2);
_LABEL(sale3);
  end;
  coVariab_Const: begin
    ExchangeP1_P2;  //Convierte a coConst_Variab
    Oper_dword_equal_dword(SetRes);
  end;
  coVariab_Variab:begin
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
    //Compara byte U
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.Uoffs, toW);
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.Uoffs, toW); //p2-p1
    _BTFSS(Z.offs, Z.bit);
    _GOTO_PEND(sale1);  //no son iguales
    //Compara byte E
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.Eoffs, toW);
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.Eoffs, toW); //p2-p1
    _BTFSS(Z.offs, Z.bit);
    _GOTO_PEND(sale2);  //no son iguales
    //Compara byte alto
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.Hoffs, toW);
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.Hoffs, toW); //p2-p1
    _BTFSS(Z.offs, Z.bit);
    _GOTO_PEND(sale3);  //no son iguales
    //Son iguales, comparar el byte bajo
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.Loffs, toW);
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.Loffs,toW);	//p2-p1
_LABEL(sale1); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
_LABEL(sale2);
_LABEL(sale3);
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROPResultExpres_bool(operType, false);   //Se pide Z para el resultado
    //Compara byte L
    _SUBWF(p1^.Loffs, toW); //p2^.L ya está en W
    _BTFSS(Z.offs, Z.bit);
    _GOTO_PEND(sale1);  //no son iguales
    //Compara byte H
    _MOVF(p1^.Hoffs, toW);
    _SUBWF(H.offs, toW); //p2-p1
    _BTFSS(Z.offs, Z.bit);
    _GOTO_PEND(sale2);  //no son iguales
    //Compara byte E
    _MOVF(p1^.Eoffs, toW);
    _SUBWF(E.offs, toW); //p2-p1
    _BTFSS(Z.offs, Z.bit);
    _GOTO_PEND(sale3);  //no son iguales
    //Comparar el byte U
    _MOVF(p1^.Uoffs, toW);
    _SUBWF(U.offs,toW);	//p2-p1
_LABEL(sale1); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
_LABEL(sale2);
_LABEL(sale3);
  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    ExchangeP1_P2;  //Convierte a coConst_Expres;
    Oper_dword_equal_dword(SetRes);
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    ExchangeP1_P2;  //Convierte a coVariab_Expres;
    Oper_dword_equal_dword(SetRes);
  end;
  coExpres_Expres:begin
    //La expresión p1, debe estar salvada y p2 en (H,W,E,U)
    p1^.SetAsVariab(GetVarDWordFromStk);
    catOperation := TCatOperation((Ord(p1^.Cat) << 2) or ord(p2^.Cat));
    //Luego el caso es similar a variable-expresión
    Oper_dword_equal_dword(SetRes);
    FreeStkRegisterdWord;
  end;
  else
    genError('Not implemented: "%s"', [CatOperationToStr]);
  end;
end;
procedure TGenCod.Oper_dword_difer_dword(SetRes: boolean);
begin
  Oper_dword_equal_dword(SetRes);
  res.Invert;
end;
procedure TGenCod.Oper_dword_add_dword(SetRes: boolean);
var
  aux: TPicRegister;
  varTmp: TxpEleVar;
begin
  case catOperation of
  coConst_Const: begin
    if p1^.valInt+p2^.valInt < $FF then begin
      //Optimiza
      SetROPResultConst_byte(p1^.valInt+p2^.valInt);
    end else if p1^.valInt+p2^.valInt < $FFFF then begin
      //Optimiza
      SetROPResultConst_word(p1^.valInt+p2^.valInt);
    end else begin
      SetROPResultConst_dword(p1^.valInt+p2^.valInt);
    end;
  end;
  coConst_Variab: begin
    SetROPResultExpres_dword(operType);
    aux := GetAuxRegisterByte;  //Pide un registro libre
    if HayError then exit;
    _movf   (p2^.Loffs,toW);
    _ADDLW  (p1^.LByte);  //Cambia C
    _movwf  (aux.offs);       //Guarda Byte L de resultado

    _movf   (p2^.Hoffs,toW);  //Prepara sumando. Altera Z, pero no toca C
    _btfsc  (STATUS,_C);      //Mira acarreo de operación anterior
    _incfsz (p2^.Hoffs,toW);
    _ADDLW  (p1^.HByte);  //Cambia C
    _movwf  (H.offs);       //Guarda Byte H de resultado

    _movf   (p2^.Eoffs,toW);  //Prepara sumando. Altera Z, pero no toca C
    _btfsc  (STATUS,_C);      //Mira acarreo de operación anterior
    _incfsz (p2^.Eoffs,toW);
    _ADDLW  (p1^.EByte);  //Cambia C
    _movwf  (E.offs);       //Guarda Byte E de resultado

    _movf   (p2^.Uoffs,toW);  //Prepara sumando. Altera Z, pero no toca C
    _btfsc  (STATUS,_C);      //Mira acarreo de operación anterior
    _incfsz (p2^.Uoffs,toW);
    _ADDLW  (p1^.UByte);
    _movwf  (U.offs);       //Guarda Byte U de resultado

    _movf (aux.offs, toW);  //Deja L en W

    aux.used := false;
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y esta en (H,W)
    if SetRes then SetROPResultExpres_dword(operType); //Se fija aquí el resultado
    //K + WHEU -> WHEU, se puede manejar como asignación con sums
    aux := GetAuxRegisterByte;  //Pide un registro libre
    _MOVWF(aux.offs);  //guarda W
    varTmp := NewTmpVarDword(aux, H, E, U);  //Crea variable temporal, con los RT
    p2^.SetAsVariab(varTmp);  //Convierte p2 a variable
    ExchangeP1_P2;  //Convierte a p1 := p1 + K;
    Oper_dword_aadd_dword(false);  //compila como autosuma
    _MOVF(aux.offs, toW);  //devuelve byet bajo en W
    aux.used := false;
    varTmp.Destroy;  //Destruye la variable
  end;
  coVariab_Const: begin
    ExchangeP1_P2;  //Convierte a coConst_Variab
    Oper_dword_add_dword(SetRes);
  end;
  coVariab_Variab:begin
    SetROPResultExpres_dword(operType);
//  Este algoritmo Falla
//    aux := GetAuxRegisterByte;  //Pide un registro libre
//    if HayError then exit;
//    _movf (p2^.Loffs,toW);
//    _addwf(p1^.Loffs,toW);
//    _movwf(aux.offs);
//    _movf (p2^.Hoffs,toW);
//    _btfsc(STATUS, _C);
//    _addlw(1);
//    _addwf(p1^.Hoffs,toW);
//    _movwf(H.offs);
//    _movf (p2^.Eoffs,toW);
//    _btfsc(STATUS, _C);
//    _addlw(1);
//    _addwf(p1^.Eoffs,toW);
//    _movwf(E.offs);
//    _movf (p2^.Uoffs,toW);
//    _btfsc(STATUS, _C);
//    _addlw(1);
//    _addwf(p1^.Uoffs,toW);
//    _movwf(U.offs);
//    _movf (aux.offs, toW);
//    aux.used := false;

    aux := GetAuxRegisterByte;  //Pide un registro libre
    if HayError then exit;
    _movf   (p2^.Loffs,toW);
    _addwf  (p1^.Loffs,toW);  //Cambia C
    _movwf  (aux.offs);       //Guarda Byte L de resultado

    _movf   (p2^.Hoffs,toW);  //Prepara sumando. Altera Z, pero no toca C
    _btfsc  (STATUS,_C);      //Mira acarreo de operación anterior
    _incfsz (p2^.Hoffs,toW);
    _addwf  (p1^.Hoffs,toW);  //Cambia C
    _movwf  (H.offs);       //Guarda Byte H de resultado

    _movf   (p2^.Eoffs,toW);  //Prepara sumando. Altera Z, pero no toca C
    _btfsc  (STATUS,_C);      //Mira acarreo de operación anterior
    _incfsz (p2^.Eoffs,toW);
    _addwf  (p1^.Eoffs,toW);  //Cambia C
    _movwf  (E.offs);       //Guarda Byte E de resultado

    _movf   (p2^.Uoffs,toW);  //Prepara sumando. Altera Z, pero no toca C
    _btfsc  (STATUS,_C);      //Mira acarreo de operación anterior
    _incfsz (p2^.Uoffs,toW);
    _addwf  (p1^.Uoffs,toW);
    _movwf  (U.offs);       //Guarda Byte U de resultado

    _movf (aux.offs, toW);  //Deja L en W

    aux.used := false;

  end;
//  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en (H,W)
//    SetROPResultExpres_word(operType);
//    aux := GetAuxRegisterByte;  //Pide un registro libre
//    if HayError then exit;
//    _BANKSEL(aux.bank);
//    _movwf(aux.offs);        //guarda byte bajo
//    _BANKSEL(p1^.bank);
//    _MOVF(p1^.Hoffs, toW);   //Carga más peso del dato 1
//    _BANKSEL(H.bank);
//    _addwf(H.offs,toF);      //Suma y guarda
//    //Siguiente byte
//    _BANKSEL(p1^.bank);
//    _MOVF(p1^.Loffs, toW);       //Carga menos peso del dato 1
//    _BANKSEL(aux.bank);
//    _addwf(aux.offs,toW);    //Suma menos peso del dato 2, deja en W
//    _btfsc(STATUS,_C);      //Hubo acarreo anterior?
//    _incf(H.offs, toF);
//    aux.used := false;
//  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,W)
    if SetRes then SetROPResultExpres_dword(operType); //Se fija aquí el resultado
    //WHEU + K -> WHEU, se puede manejar como asignación con suma
    aux := GetAuxRegisterByte;  //Pide un registro libre
    _MOVWF(aux.offs);  //gaurda W
    varTmp := NewTmpVarDword(aux, H, E, U);  //Crea variable temporal
    p1^.SetAsVariab(varTmp);  //Convierte p1 a variable
    Oper_dword_aadd_dword(false);  //compila como autosuma
    _MOVF(aux.offs, toW);  //devuelve byet bajo en W
    aux.used := false;
    varTmp.Destroy;  //Destruye la variable
  end;
//  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,W)
//    SetROPResultExpres_word(operType);
//    aux := GetAuxRegisterByte;  //Pide un registro libre
//    if HayError then exit;
//    _movwf(aux.offs);      //guarda byte bajo
//    _BANKSEL(p2^.bank);
//    _MOVF(p2^.Hoffs, toW);     //Carga más peso del dato 1
//    _BANKSEL(H.bank);
//    _addwf(H.offs,toF);    //Suma y guarda
//    _BANKSEL(p2^.bank);
//    _MOVF(p2^.Loffs, toW);     //Carga menos peso del dato 1
//    _BANKSEL(aux.bank);
//    _addwf(aux.offs,toW);  //Suma menos peso del dato 2, deja en W
//    _BANKSEL(H.bank);
//    _btfsc(STATUS,_C);    //Hubo acarreo anterior?
//    _incf(H.offs, toF);
//    aux.used := false;
//  end;
//  coExpres_Expres:begin
//    SetROPResultExpres_word(operType);
//    //p1 está salvado en pila y p2 en (_H,W)
//    p1^.Cat := coVariab;  //Convierte a variable
//    p1^.rVar := GetVarWordFromStk;
//    catOperation := TCatOperation((Ord(p1^.Cat) << 2) or ord(p2^.Cat));
//    //Luego el caso es similar a coVariab_Expres
//    Oper_word_add_word;
//    FreeStkRegisterByte(spH);   //libera pila, obtiene dirección
//    FreeStkRegisterByte(spL);   //libera pila, obtiene dirección
//  end;
  else
    genError('Not implemented: "%s"', [CatOperationToStr]);
  end;
end;
procedure TGenCod.Oper_dword_aadd_dword(SetRes: boolean);
begin
  if p1^.Cat <> coVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.Cat of
  coConst : begin
    if SetRes then SetROPResultExpres_dword(operType);  //Realmente, el resultado no es importante
    if p2^.valInt = 0 then begin
      //No cambia
    end else if p2^.valInt <= $FF then begin
      _movlw (p2^.LByte);
      _addwf (p1^.Loffs,toF);
      _btfsc (STATUS,_C);
      _INCF  (p1^.Hoffs,toF);
      _btfsc (STATUS,_Z);
      _INCF  (p1^.Eoffs,toF);
      _btfsc (STATUS,_Z);
      _INCF  (p1^.Uoffs,toF);
    end else if p2^.valInt <= $FFFF then begin
      _movlw (p2^.LByte);
      _addwf (p1^.Loffs,toF);
      _movlw (p2^.HByte);
      _btfsc (STATUS,_C);
      _ADDLW (1);
      _addwf (p1^.Hoffs,toF);
      _btfsc (STATUS,_C);
      _INCF  (p1^.Eoffs,toF);
      _btfsc (STATUS,_Z);
      _INCF  (p1^.Uoffs,toF);
    end else begin
      _movlw (p2^.LByte);
      _addwf (p1^.Loffs,toF);
      _movlw (p2^.HByte);
      _btfsc (STATUS,_C);
      _ADDLW (1);
      _addwf (p1^.Hoffs,toF);
      _movlw (p2^.EByte);
      _btfsc (STATUS,_C);
      _ADDLW (1);
      _addwf (p1^.Eoffs,toF);
      _movlw (p2^.UByte);
      _btfsc (STATUS,_C);
      _ADDLW (1);
      _addwf (p1^.Uoffs,toF);
    end;
  end;
  coVariab: begin
    if SetRes then SetROPResultExpres_dword(operType);  //Realmente, el resultado no es importante
    _movf   (p2^.Loffs,toW);
    _addwf  (p1^.Loffs,toF);
    _movf   (p2^.Hoffs,toW);
    _btfsc  (STATUS,_C);
    _incfsz (p2^.Hoffs,toW);
    _addwf  (p1^.Hoffs,toF);
    _movf   (p2^.Eoffs,toW);
    _btfsc  (STATUS,_C);
    _incfsz (p2^.Eoffs,toW);
    _addwf  (p1^.Eoffs,toF);
    _movf   (p2^.Uoffs,toW);
    _btfsc  (STATUS,_C);
    _incfsz (p2^.Uoffs,toW);
    _addwf  (p1^.Uoffs,toF);
  end;
  coExpres: begin   //Se asume que está en U,E,H,w
    if SetRes then SetROPResultExpres_dword(operType);  //Realmente, el resultado no es importante
    _addwf  (p1^.Loffs,toF);  //p2 ya está en W
    _movf   (H.offs,toW);
    _btfsc  (STATUS,_C);
    _incfsz (H.offs,toW);
    _addwf  (p1^.Hoffs,toF);
    _movf   (E.offs,toW);
    _btfsc  (STATUS,_C);
    _incfsz (E.offs,toW);
    _addwf  (p1^.Eoffs,toF);
    _movf   (U.offs,toW);
    _btfsc  (STATUS,_C);
    _incfsz (U.offs,toW);
    _addwf  (p1^.Uoffs,toF);
  end;
  else
    GenError('No soportado'); exit;
  end;
end;
//////////// Operaciones con Char
procedure TGenCod.Oper_char_asig_char(SetRes: boolean);
begin
  if p1^.Cat <> coVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.Cat of
  coConst : begin
    SetROPResultExpres_char(operType);  //Realmente, el resultado no es importante
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
    SetROPResultExpres_char(operType);  //Realmente, el resultado no es importante
    _BANKSEL(p2^.bank);  //verifica banco destino
    _MOVF(p2^.offs, toW);
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVWF(p1^.offs);
  end;
  coExpres: begin  //ya está en w
    SetROPResultExpres_char(operType);  //Realmente, el resultado no es importante
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVWF(p1^.offs);
  end;
  else
    GenError('No soportado'); exit;
  end;
end;
procedure TGenCod.Oper_char_equal_char(SetRes: boolean);
begin
  Oper_byte_equal_byte(SetRes);  //es lo mismo
end;
procedure TGenCod.Oper_char_difer_char(SetRes: boolean);
begin
  Oper_byte_difer_byte(SetRes); //es lo mismo
end;
/////////////funciones del sistema
procedure TGenCod.codif_1mseg;
//Codifica rutina de reatrdo de 1mseg.
begin
  PutFwdComm(';inicio rutina 1 mseg.');
  if _CLOCK = 1000000 then begin
    _MOVLW(62);  //contador de iteraciones
    _ADDLW(255);  //lazo de 4 ciclos
    _BTFSS(STATUS,_Z);
    _GOTO(_PC-2); PutComm(';fin rutina 1 mseg a 1MHz.');
  end else if _CLOCK = 2000000 then begin
    _MOVLW(125);  //contador de iteraciones
    _ADDLW(255);  //lazo de 4 ciclos
    _BTFSS(STATUS,_Z);
    _GOTO(_PC-2); PutComm(';fin rutina 1 mseg a 2MHz.');
  end else if _CLOCK = 4000000 then begin
    //rtuina básica para 4MHz
    _MOVLW(250);  //contador de iteraciones
    _ADDLW(255);  //lazo de 4 ciclos
    _BTFSS(STATUS,_Z);
    _GOTO(_PC-2); PutComm(';fin rutina 1 mseg a 4MHz.');
  end else if _CLOCK = 8000000 then begin
    _MOVLW(250);
    _ADDLW(255);   //lazo de 8 ciclos
    _GOTO(_PC+1);  //introduce 4 ciclos más de retardo
    _GOTO(_PC+1);
    _BTFSS(STATUS,_Z);
    _GOTO(_PC-4); PutComm(';fin rutina 1 mseg a 8Mhz.');
  end else if _CLOCK = 10000000 then begin
    _MOVLW(250);
    _ADDLW(255);   //lazo de 10 ciclos
    _GOTO(_PC+1);  //introduce 6 ciclos más de retardo
    _GOTO(_PC+1);
    _GOTO(_PC+1);
    _BTFSS(STATUS,_Z);
    _GOTO(_PC-5); PutComm(';fin rutina 1 mseg a 10MHz.');
  end else if _CLOCK = 12000000 then begin
    _MOVLW(250);
    _ADDLW(255);   //lazo de 12 ciclos
    _GOTO(_PC+1);  //introduce 8 ciclos más de retardo
    _GOTO(_PC+1);
    _GOTO(_PC+1);
    _GOTO(_PC+1);
    _BTFSS(STATUS,_Z);
    _GOTO(_PC-6); PutComm(';fin rutina 1 mseg a 12MHz.');
  end else if _CLOCK = 16000000 then begin
    _MOVLW(250);
    _ADDLW(255);   //lazo de 16 ciclos
    _GOTO(_PC+1);  //introduce 12 ciclos más de retardo
    _GOTO(_PC+1);
    _GOTO(_PC+1);
    _GOTO(_PC+1);
    _GOTO(_PC+1);
    _GOTO(_PC+1);
    _BTFSS(STATUS,_Z);
    _GOTO(_PC-8); PutComm(';fin rutina 1 mseg a 12MHz.');
  end else if _CLOCK = 20000000 then begin
    _MOVLW(250);
    _ADDLW(255);   //lazo de 20 ciclos
    _GOTO(_PC+1);  //introduce 16 ciclos más de retardo
    _GOTO(_PC+1);
    _GOTO(_PC+1);
    _GOTO(_PC+1);
    _GOTO(_PC+1);
    _GOTO(_PC+1);
    _GOTO(_PC+1);
    _GOTO(_PC+1);
    _BTFSS(STATUS,_Z);
    _GOTO(_PC-10); PutComm(';fin rutina 1 mseg a 12MHz.');
  end else begin
    GenError('Clock frequency %d not supported for delay_ms().', [_CLOCK]);
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
  //aux := GetAuxRegisterByte;  //Pide un registro libre
  aux := FSR;  //Usa el FSR como registro auxiliar
  if HayError then exit;
  {Esta rutina recibe los milisegundos en los registros en (H,w) o en (w)
  En cualquier caso, siempre usa el registros H , el acumulador "w" y un reg. auxiliar.
  Se supone que para pasar los parámetros, ya se requirió H, así que no es necesario
  crearlo.}
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
  //aux.used := false;  //libera registro
end;
procedure TGenCod.fun_delay_ms(fun: TxpEleFun);
begin
  if not CaptureTok('(') then exit;
  GetExpressionE(0, pexPARSY);  //captura parámetro
  if HayError then exit;   //aborta
  //Se terminó de evaluar un parámetro
  res.LoadToReg;   //Carga en registro de trabajo
  if HayError then exit;
  if res.Typ = typByte then begin
    //El parámetro byte, debe estar en W
    _CALL(fun.adrr);
  end else if res.Typ = typWord then begin
    //El parámetro word, debe estar en (H, W)
    _CALL(fun.adrr+1);
  end else begin
    GenError('Invalid parameter type: %s', [res.Typ.name]);
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
    if curBlk.idClass = eltFunc then begin
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
  curFunTyp: TxpEleType;
  curBlk: TxpElement;
  curFun: TxpEleFun;
//  adrReturn: word;
begin
  curBlk := TreeElems.curNode.Parent;  //El curNode, debe ser de tipo "Body".
  if curBlk.idClass = eltMain then begin  //En el programa principal
    _SLEEP;   //Así se termina un programa en PicPas
    exit;
  end;
  //curBlk debe ser de tipo TxpEleFun
  curFun := TxpEleFun(curBlk);
  curFunTyp := curFun.typ;
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
  if curFunTyp <> res.Typ then begin
    GenError('Expected a "%s" expression.', [curFunTyp.name]);
  end;
  res.LoadToReg;
  res.SetAsNull;  //No es función
  CodifRETURN(curBlk);  //Codifica salto
end;
procedure TGenCod.fun_Inc(fun: TxpEleFun);
begin
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Cat of  //el parámetro debe estar en "res"
  coConst : begin
    GenError('Cannot increase a constant.'); exit;
  end;
  coVariab: begin
    if (res.Typ = typByte) or (res.Typ = typChar) then begin
      _INCF(res.offs, toF);
    end else if res.Typ = typWord then begin
      _INCF(res.Loffs, toF);
      _BTFSC(STATUS, _Z);
      _INCF(res.Hoffs, toF);
    end else if res.Typ = typDWord then begin
      _INCF(res.Loffs, toF);
      _BTFSC(STATUS, _Z);
      _INCF(res.Hoffs, toF);
      _BTFSC(STATUS, _Z);
      _INCF(res.Eoffs, toF);
      _BTFSC(STATUS, _Z);
      _INCF(res.Uoffs, toF);
    end else begin
      GenError('Invalid parameter type: %s', [res.Typ.name]);
      exit;
    end;
  end;
  coExpres: begin  //se asume que ya está en (_H,w)
    GenError('Cannot increase an expression.'); exit;
  end;
  end;
  res.SetAsNull;  //No es función
  //Verifica fin de parámetros
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Dec(fun: TxpEleFun);
begin
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Cat of  //el parámetro debe estar en "res"
  coConst : begin
    GenError('Cannot decrease a constant.'); exit;
  end;
  coVariab: begin
    if (res.Typ = typByte) or (res.Typ = typChar) then begin
      _DECF(res.offs, toF);
    end else if res.Typ = typWord then begin
      _MOVF(res.Loffs, toW);
      _BTFSC(STATUS, _Z);
      _DECF(res.Hoffs, toF);
      _DECF(res.Loffs, toF);
    end else if res.Typ = typDWord then begin
      _MOVLW(1);
      _subwf(res.Loffs, toF);
      _BTFSS(STATUS, _C);
      _subwf(RES.Hoffs, toF);
      _BTFSS(STATUS, _C);
      _subwf(RES.Eoffs, toF);
      _BTFSS(STATUS, _C);
      _subwf(RES.Uoffs, toF);
    end else begin
      GenError('Invalid parameter type: %s', [res.Typ.name]);
      exit;
    end;
  end;
  coExpres: begin  //se asume que ya está en (_H,w)
    GenError('Cannot decrease an expression.'); exit;
  end;
  end;
  res.SetAsNull;  //No es función
  //Verifica fin de parámetros
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Ord(fun: TxpEleFun);
var
  tmpVar: TxpEleVar;
begin
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Cat of  //el parámetro debe estar en "res"
  coConst : begin
    if res.Typ = typChar then begin
      SetResultConst(typByte);  //Solo cambia el tipo, no el valor
      //No se usa SetROPResultConst_Byte, porque no estamos en ROP
    end else begin
      GenError('Cannot convert to ordinal.'); exit;
    end;
  end;
  coVariab: begin
    if res.Typ = typChar then begin
      //Sigue siendo variable
      tmpVar := CreateTmpVar('', typByte);   //crea variable temporal Byte
      tmpVar.adrByte0.Assign(res.rVar.adrByte0); //apunta al mismo byte
      SetResultVariab(tmpVar);  //Actualiza "res"
    end else begin
      GenError('Cannot convert to ordinal.'); exit;
    end;
  end;
  coExpres: begin  //se asume que ya está en (w)
    if res.Typ = typChar then begin
      //Es la misma expresión, solo que ahora es Byte.
      res.SetAsExpres(typByte); //No se puede usar SetROPResultExpres_byte, porque no hay p1 y p2
    end else begin
      GenError('Cannot convert to ordinal.'); exit;
    end;
  end;
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Chr(fun: TxpEleFun);
var
  tmpVar: TxpEleVar;
begin
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Cat of  //el parámetro debe estar en "res"
  coConst : begin
    if res.Typ = typByte then begin
      SetResultConst(typChar);  //Solo cambia el tipo, no el valor
      //No se usa SetROPResultConst_Char, porque no estamos en ROP
    end else begin
      GenError('Cannot convert to char.'); exit;
    end;
  end;
  coVariab: begin
    if res.Typ = typByte then begin
      //Sigue siendo variable
      tmpVar := CreateTmpVar('', typChar);   //crea variable temporal
      tmpVar.adrByte0.Assign(res.rVar.adrByte0); //apunta al mismo byte
      SetResultVariab(tmpVar);
    end else begin
      GenError('Cannot convert to char.'); exit;
    end;
  end;
  coExpres: begin  //se asume que ya está en (w)
    if res.Typ = typByte then begin
      //Es la misma expresión, solo que ahora es Char.
      res.SetAsExpres(typChar); //No se puede usar SetROPResultExpres_char, porque no hay p1 y p2;
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
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Cat of  //el parámetro debe estar en "res"
  coConst : begin
    if res.Typ = typByte then begin
      if res.valInt= 0 then begin
        SetResultConst(typBit);  //No se usa SetROPResultConst_bit, porque no estamos en ROP
        res.valBool := false;
      end else begin
        SetResultConst(typBit);  //No se usa SetROPResultConst_bit, porque no estamos en ROP
        res.valBool := true;
      end;
    end else begin
      GenError('Cannot convert to bit.'); exit;
    end;
  end;
  coVariab: begin
    if res.Typ = typByte then begin
      SetResultExpres(typBit); //No se puede usar SetROPResultExpres_bit, porque no hay p1 y p2;
      res.Inverted := true;
      //Se asumirá que cualquier valor diferente de cero, devuelve 1
      _MOVF(res.Loffs, toW);    //el resultado aparecerá en Z, invertido
      {Notar que se ha usado res.Loff, para apuntar a la dirección de la variable byte.
      Si se hubeise usado solo res.off, apuntaría a una dirección del tipo bit}
    end else begin
      GenError('Cannot convert to bit.'); exit;
    end;
  end;
  coExpres: begin  //se asume que ya está en (w)
    if res.Typ = typByte then begin
      SetResultExpres(typBit); //No se puede usar SetROPResultExpres_bit, porque no hay p1 y p2;
      res.Inverted := true;
      _ADDLW(0);   //el resultado aparecerá en Z, invertido
    end else begin
      GenError('Cannot convert to bit.'); exit;
    end;
  end;
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Byte(fun: TxpEleFun);
var
  tmpVar: TxpEleVar;
begin
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Cat of  //el parámetro debe estar en "res"
  coConst : begin
    if res.Typ = typByte then begin
      //ya es Byte
    end else if res.Typ = typChar then begin
      res.SetAsConst(typByte);  //Solo cambia el tipo
    end else if res.Typ = typWord then begin
      res.SetAsConst(typByte);  //Cambia el tipo
      res.valInt := res.valInt and $FF;
    end else if res.Typ = typDWord then begin
      res.SetAsConst(typByte);  //Cambia el tipo
      res.valInt := res.valInt and $FF;
    end else if (res.Typ = typBool) or (res.Typ = typBit) then begin
      res.SetAsConst(typByte);  //Cambia el tipo
      if res.valBool then res.valInt := 1 else res.valInt := 0;
    end else begin
      GenError('Cannot convert to byte.'); exit;
    end;
  end;
  coVariab: begin
    if res.Typ = typBit then begin
      SetResultExpres(typByte);
      _CLRW;
      _BTFSC(res.Boffs, res.bit);
      _MOVLW(1);  //devuelve 1
      //Es lo mismo
    end else if res.Typ = typChar then begin
      //Crea varaible que apunte al byte bajo
      tmpVar := CreateTmpVar('', typByte);   //crea variable temporal Byte
      tmpVar.adrByte0.Assign(res.rVar.adrByte0); //apunta al mismo byte
      SetResultVariab(tmpVar);
    end else if res.Typ = typByte then begin
      //Es lo mismo
    end else if res.Typ = typWord then begin
      //Crea varaible que apunte al byte bajo
      tmpVar := CreateTmpVar('', typByte);   //crea variable temporal Byte
      tmpVar.adrByte0.Assign(res.rVar.adrByte0); //apunta al byte bajo
      SetResultVariab(tmpVar);
    end else if res.Typ = typDWord then begin
      //CRea varaible que apunte al byte bajo
      tmpVar := CreateTmpVar('', typByte);   //crea variable temporal Byte
      tmpVar.adrByte0.Assign(res.rVar.adrByte0); //apunta al byte bajo
      SetResultVariab(tmpVar);
    end else begin
      GenError('Cannot convert to byte.'); exit;
    end;
  end;
  coExpres: begin  //se asume que ya está en (w)
    if res.Typ = typByte then begin
      //Ya está en W
      //Ya es Byte
    end else if res.Typ = typChar then begin
      //Ya está en W
      res.SetAsExpres(typByte);  //Solo cambia el tipo
    end else if res.Typ = typWord then begin
      //Ya está en W el byte bajo
      res.SetAsExpres(typByte);  //Cambia el tipo
    end else if res.Typ = typDWord then begin
      //Ya está en W el byet bajo
      res.SetAsExpres(typByte);  //Cambia el tipo
    end else if (res.Typ = typBool) or (res.Typ = typBit) then begin
      _MOVLW(0);    //Z -> W
      _BTFSC(STATUS, _Z);
      _MOVLW(1);
      res.SetAsExpres(typByte);  //Cambia el tipo
    end else begin
      GenError('Cannot convert to byte.'); exit;
    end;
  end;
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Word(fun: TxpEleFun);
var
  tmpVar: TxpEleVar;
begin
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Cat of  //el parámetro debe estar en "res"
  coConst : begin
    if res.Typ = typByte then begin
      res.SetAsConst(typWord);  //solo cambia el tipo
    end else if res.Typ = typChar then begin
      res.SetAsConst(typWord);  //solo cambia el tipo
    end else if res.Typ = typWord then begin
      //ya es Word
    end else if res.Typ = typDWord then begin
      res.SetAsConst(typWord);
      res.valInt := res.valInt and $FFFF;
    end else if (res.Typ = typBool) or (res.Typ = typBit) then begin
      res.SetAsConst(typWord);
      if res.valBool then res.valInt := 1 else res.valInt := 0;
    end else begin
      GenError('Cannot convert this constant to word.'); exit;
    end;
  end;
  coVariab: begin
    typWord.DefineRegister;
    if res.Typ = typByte then begin
      SetResultExpres(typWord);  //No podemos devolver variable. Pero sí expresión
      _CLRF(H.offs);
      _MOVF(res.offs, toW);
    end else if res.Typ = typChar then begin
      SetResultExpres(typWord);  //No podemos devolver variable. Pero sí expresión
      _CLRF(H.offs);
      _MOVF(res.offs, toW);
    end else if res.Typ = typWord then begin
      //ya es Word
    end else if res.Typ = typDWord then begin
      //Crea varaible que apunte al word bajo
      tmpVar := CreateTmpVar('', typWord);   //crea variable temporal Word
      tmpVar.adrByte0.Assign(res.rVar.adrByte0); //apunta al byte L
      tmpVar.adrByte1.Assign(res.rVar.adrByte1); //apunta al byte H
      SetResultVariab(tmpVar);
    end else if (res.Typ = typBool) or (res.Typ = typBit) then begin
      SetResultExpres(typWord);  //Devolvemo expresión
      _CLRF(H.offs);
      _MOVLW(0);
      _BTFSC(STATUS, _Z);
      _MOVLW(1);
    end else begin
      GenError('Cannot convert this variable to word.'); exit;
    end;
  end;
  coExpres: begin  //se asume que ya está en (w)
    typWord.DefineRegister;
    if res.Typ = typByte then begin
      res.SetAsExpres(typWord);
      //Ya está en W el byte bajo
      _CLRF(H.offs);
    end else if res.Typ = typChar then begin
      res.SetAsExpres(typWord);
      //Ya está en W el byte bajo
      _CLRF(H.offs);
    end else if res.Typ = typWord then begin
//      Ya es word
    end else if res.Typ = typDWord then begin
      res.SetAsExpres(typWord);
      //Ya está en H,W el word bajo
    end else if (res.Typ = typBool) or (res.Typ = typBit) then begin
      res.SetAsExpres(typWord);
      _CLRF(H.offs);
      _MOVLW(0);    //Z -> W
      _BTFSC(STATUS, _Z);
      _MOVLW(1);
    end else begin
      GenError('Cannot convert expression to word.'); exit;
    end;
  end;
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_DWord(fun: TxpEleFun);
begin
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Cat of  //el parámetro debe estar en "res"
  coConst : begin
    if res.Typ = typByte then begin
      res.SetAsConst(typDWord);  //Solo cambia el tipo
    end else if res.Typ = typChar then begin
      res.SetAsConst(typDWord);  //Solo cambia el tipo
    end else if res.Typ = typWord then begin
      res.SetAsConst(typDWord);  //Solo cambia el tipo
    end else if res.Typ = typDWord then begin
      //ya es DWord
    end else if (res.Typ = typBool) or (res.Typ = typBit) then begin
      res.SetAsConst(typDWord);  //Solo cambia el tipo
      if res.valBool then res.valInt := 1 else res.valInt := 0;
    end else begin
      GenError('Cannot convert this constant to Dword.'); exit;
    end;
  end;
  coVariab: begin
    typDword.DefineRegister;
    if res.Typ = typByte then begin
      SetResultExpres(typDWord);  //No podemos devolver variable. Pero sí expresión
      _CLRF(U.offs);
      _CLRF(E.offs);
      _CLRF(H.offs);
      _MOVF(res.offs, toW);
    end else if res.Typ = typChar then begin
      SetResultExpres(typDWord);  //No podemos devolver variable. Pero sí expresión
      _CLRF(U.offs);
      _CLRF(E.offs);
      _CLRF(H.offs);
      _MOVF(res.offs, toW);
    end else if res.Typ = typWord then begin
      SetResultExpres(typDWord);  //No podemos devolver variable. Pero sí expresión
      _CLRF(U.offs);
      _CLRF(E.offs);
      _MOVF(res.Hoffs, toW);
      _MOVWF(H.offs);
      _MOVF(res.Loffs, toW);
    end else if res.Typ = typDWord then begin
      //ya es Word. Lo deja como varaible DWord
    end else if (res.Typ = typBool) or (res.Typ = typBit) then begin
      SetResultExpres(typDWord);  //No podemos devolver variable. Pero sí expresión
      _CLRF(U.offs);
      _CLRF(E.offs);
      _CLRF(H.offs);
      _MOVLW(0);    //Z -> W
      _BTFSC(STATUS, _Z);
      _MOVLW(1);
    end else begin
      GenError('Cannot convert this variable to Dword.'); exit;
    end;
  end;
  coExpres: begin  //se asume que ya está en (w)
    typDword.DefineRegister;
    if res.Typ = typByte then begin
      res.SetAsExpres(typDWord);  //No podemos devolver variable. Pero sí expresión
      //Ya está en W el byte bajo
      _CLRF(U.offs);
      _CLRF(E.offs);
      _CLRF(H.offs);
    end else if res.Typ = typChar then begin
      res.SetAsExpres(typDWord);  //No podemos devolver variable. Pero sí expresión
      //Ya está en W el byte bajo
      _CLRF(U.offs);
      _CLRF(E.offs);
      _CLRF(H.offs);
    end else if res.Typ = typWord then begin
      res.SetAsExpres(typDWord);  //No podemos devolver variable. Pero sí expresión
      //Ya está en H,W el word
      _CLRF(U.offs);
      _CLRF(E.offs);
    end else if res.Typ = typDWord then begin
//      Ya es Dword
    end else if (res.Typ = typBool) or (res.Typ = typBit) then begin
      res.SetAsExpres(typDWord);  //No podemos devolver variable. Pero sí expresión
      _CLRF(U.offs);
      _CLRF(E.offs);
      _CLRF(H.offs);
      _MOVLW(0);    //Z -> W
      _BTFSC(STATUS, _Z);
      _MOVLW(1);
    end else begin
      GenError('Cannot convert expression to Dword.'); exit;
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
  case res.Cat of  //el parámetro debe estar en "res"
  coConst : begin
    GenError('PORT or BIT variable expected.'); exit;
  end;
  coVariab: begin
    if res.Typ = typByte then begin
      //Se asume que será algo como PORTA, PORTB, ...
      _MOVLW($FF);   //todos como entradas
      _BANKSEL(1);   //los registros TRIS, están en el banco 1
      _MOVWF(res.offs); //escribe en TRIS
    end else if res.Typ = typBit then begin
      //Se asume que será algo como PORTA.0, PORTB.0, ...
      _BANKSEL(1);   //los registros TRIS, están en el banco 1
      _BSF(res.offs, res.bit); //escribe en TRIS
    end else begin
      GenError('Invalid type.'); exit;
    end;
    res.SetAsNull; //No es función así que no es necesario fijar el resultado
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
  case res.Cat of  //el parámetro debe estar en "res"
  coConst : begin
    GenError('PORT variable expected.'); exit;
  end;
  coVariab: begin
    if res.Typ = typByte then begin
      //Se asume que será algo como PORTA, PORTB, ...
      _BANKSEL(1);   //los registros TRIS, están en el banco 1
      _CLRF(res.offs); //escribe en TRIS
    end else if res.Typ = typBit then begin
      //Se asume que será algo como PORTA.0, PORTB.0, ...
      _BANKSEL(1);   //los registros TRIS, están en el banco 1
      _BCF(res.offs, res.bit); //escribe en TRIS
    end else begin
      GenError('Invalid type.'); exit;
    end;
    res.SetAsNull; //No es función así que no es necesario fijar el resultado
  end;
  coExpres: begin  //se asume que ya está en (w)
    GenError('PORT variable expected.'); exit;
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
  case res.Cat of  //el parámetro debe estar en "res"
  coConst : begin
    if (res.Typ = typByte) or (res.Typ = typWord) or (res.Typ = typDWord) then begin
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
  xLex.AddIdentSpecList('THEN var type absolute interrupt', tnKeyword);
  xLex.AddIdentSpecList('program public private method const', tnKeyword);
  xLex.AddIdentSpecList('class create destroy sub do begin', tnKeyword);
  xLex.AddIdentSpecList('END ELSE ELSIF UNTIL', tnBlkDelim);
  xLex.AddIdentSpecList('true false', tnBoolean);
  xLex.AddIdentSpecList('if while repeat for', tnStruct);
  xLex.AddIdentSpecList('and or xor not div mod in', tnOperator);
  xLex.AddIdentSpecList('umulword', tnOperator);
  //tipos predefinidos
  xLex.AddIdentSpecList('bit boolean byte word char dword', tnType);
  //funciones del sistema
  xLex.AddIdentSpecList('exit delay_ms Inc Dec Ord Chr', tnSysFunct);
  xLex.AddIdentSpecList('SetAsInput SetAsOutput SetBank', tnSysFunct);
  //símbolos especiales
  xLex.AddSymbSpec('+',  tnOperator);
  xLex.AddSymbSpec('+=', tnOperator);
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
  xLex.AddSymbSpec('[',  tnOthers);
  xLex.AddSymbSpec(']',  tnOthers);
  //crea tokens delimitados
  xLex.DefTokDelim('''','''', tnString);
  xLex.DefTokContent('[#]','[0-9]*', tnChar);
//  xLex.DefTokDelim('"','"', tnString);

  xLex.DefTokDelim('//','', xLex.tnComment);
  xLex.DefTokDelim('{','}', xLex.tnComment, tdMulLin);
  xLex.DefTokDelim('(\*','\*)', xLex.tnComment, tdMulLin);
  xLex.DefTokDelim('{$','}', tnDirective, tdUniLin);
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

  {Los operadores deben crearse con su precedencia correcta
  Precedencia de operadores en Pascal:
  6)    ~, not, signo "-"   (mayor precedencia)
  5)    *, /, div, mod, and, shl, shr, &
  4)    |, !, +, -, or, xor
  3)    =, <>, <, <=, >, >=, in
  2)    :=                  (menor precedencia)
  }
  //////////////////////////////////////////
  //////// Operaciones con Bit ////////////

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

  //////////////////////////////////////////
  //////// Operaciones con Boolean ////////////
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
  //////////////////////////////////////////
  //////// Operaciones con Byte ////////////
  {Los operadores deben crearse con su precedencia correcta}

  opr:=typByte.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typByte,@Oper_byte_asig_byte);
  opr:=typByte.CreateBinaryOperator('+',4,'add');  //suma
  opr.CreateOperation(typByte,@Oper_byte_add_byte);
  opr.CreateOperation(typWord,@Oper_byte_add_word);
  opr:=typByte.CreateBinaryOperator('-',4,'subs');  //suma
  opr.CreateOperation(typByte,@Oper_byte_sub_byte);
  opr:=typByte.CreateBinaryOperator('*',5,'mult');  //byte*byte -> word
  opr.CreateOperation(typByte,@Oper_byte_mul_byte);
  opr:=typByte.CreateBinaryOperator('DIV',5,'div');  //byte / byte ->byte
  opr.CreateOperation(typByte,@Oper_byte_div_byte);

  opr:=typByte.CreateBinaryOperator('AND',5,'and');  //suma
  opr.CreateOperation(typByte,@Oper_byte_and_byte);
  opr.CreateOperation(typBit ,@Oper_byte_and_bit);
  opr:=typByte.CreateBinaryOperator('OR',4,'or');  //suma
  opr.CreateOperation(typByte,@Oper_byte_or_byte);
  opr.CreateOperation(typBit,@Oper_byte_or_bit);
  opr:=typByte.CreateBinaryOperator('XOR',4,'xor');  //suma
  opr.CreateOperation(typByte,@Oper_byte_xor_byte);
  opr.CreateOperation(typBit,@Oper_byte_xor_bit);

  opr:=typByte.CreateUnaryPreOperator('NOT', 6, 'not', @Oper_not_byte);

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

  opr:=typByte.CreateBinaryOperator('>>',5,'shr');  { TODO : Definir bien la precedencia }
  opr.CreateOperation(typByte,@Oper_byte_shr_byte);
  opr:=typByte.CreateBinaryOperator('<<',5,'shl');
  opr.CreateOperation(typByte,@Oper_byte_shl_byte);
  //////////////////////////////////////////
  //////// Operaciones con Char ////////////
  {Los operadores deben crearse con su precedencia correcta}
  opr:=typChar.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typChar,@Oper_char_asig_char);
  opr:=typChar.CreateBinaryOperator('=',3,'equal');  //asignación
  opr.CreateOperation(typChar,@Oper_char_equal_char);
  opr:=typChar.CreateBinaryOperator('<>',3,'difer');  //asignación
  opr.CreateOperation(typChar,@Oper_char_difer_char);

  //////////////////////////////////////////
  //////// Operaciones con Word ////////////
  {Los operadores deben crearse con su precedencia correcta}

  opr:=typWord.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typWord,@Oper_word_asig_word);
  opr.CreateOperation(typByte,@Oper_word_asig_byte);

  opr:=typWord.CreateBinaryOperator('=',3,'equal');  //igualdad
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

  opr:=typWord.CreateBinaryOperator('AND', 5, 'and');  //AND
  opr.CreateOperation(typByte, @Oper_word_and_byte);

  opr:=typWord.CreateBinaryOperator('UMULWORD',5,'umulword');  //suma
  opr.CreateOperation(typWord,@Oper_word_umulword_word);

  //////////////////////////////////////////
  //////// Operaciones con DWord ////////////
  {Los operadores deben crearse con su precedencia correcta}
  opr:=typDWord.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typDWord,@Oper_dword_asig_dword);
  opr.CreateOperation(typWord,@Oper_dword_asig_word);
  opr.CreateOperation(typByte,@Oper_dword_asig_byte);

  opr:=typDWord.CreateBinaryOperator('=',3,'equal');  //igualdad
  opr.CreateOperation(typDWord,@Oper_dword_equal_dword);
  opr:=typDWord.CreateBinaryOperator('<>',3,'difer');
  opr.CreateOperation(typDWord,@Oper_dword_difer_dword);

  opr:=typDWord.CreateBinaryOperator('+=',2,'asuma');  //suma
  opr.CreateOperation(typDWord,@Oper_dword_aadd_dword);

  opr:=typDWord.CreateBinaryOperator('+',4,'suma');  //suma
  opr.CreateOperation(typDWord,@Oper_dword_add_dword);
//  opr.CreateOperation(typByte,@Oper_word_add_byte);

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
  //Funciones INLINE
  f := CreateSysFunction('exit'     , nil, @fun_Exit);
  f := CreateSysFunction('Inc'      , nil, @fun_Inc);
  f := CreateSysFunction('Dec'      , nil, @fun_Dec);
  f := CreateSysFunction('Ord'      , @callParam, @fun_Ord);
  f := CreateSysFunction('Chr'      , @callParam, @fun_Chr);
  f := CreateSysFunction('Bit'      , @callParam, @fun_Bit);
  f := CreateSysFunction('Byte'     , @callParam, @fun_Byte);
  f := CreateSysFunction('Word'     , @callParam, @fun_Word);
  f := CreateSysFunction('DWord'    , @callParam, @fun_DWord);
  f := CreateSysFunction('SetAsInput' ,nil, @fun_SetAsInput);
  f := CreateSysFunction('SetAsOutput',nil, @fun_SetAsOutput);
  f := CreateSysFunction('SetBank'  , nil, @fun_SetBank);
  //Funciones de sistema para operaciones aritméticas/lógicas complejas
  //Multiplicación byte por byte a word
  f_byte_mul_byte_16 := CreateSysFunction('byte_mul_byte_16', nil, nil);
  f_byte_mul_byte_16.adrr:=$0;
  f_byte_mul_byte_16.compile := @byte_mul_byte_16;
  f_byte_mul_byte_16.OnAddCaller := @AddCaller;  //Para que lleve la cuenta de las llamadas a subrutinas
  //Multiplicación byte DIV, MOD byte a byte
  f_byte_div_byte := CreateSysFunction('byte_div_byte', nil, nil);
  f_byte_div_byte.adrr:=$0;
  f_byte_div_byte.compile := @byte_div_byte;
  f_byte_div_byte.OnAddCaller := @AddCaller;  //Para que lleve la cuenta de las llamadas a subrutinas
  //Multiplicación word por word a word
  f_word_mul_word_16 := CreateSysFunction('word_mul_word_16', nil, nil);
  f_word_mul_word_16.adrr:=$0;
  f_word_mul_word_16.compile := @word_mul_word_16;
  f_word_mul_word_16.OnAddCaller := @AddCaller;  //Para que lleve la cuenta de las llamadas a subrutinas
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
//5442
