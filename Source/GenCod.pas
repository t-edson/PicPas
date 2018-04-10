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
2. El acumulador W         -> Si el resultado es de tipo byte o char.
3. Los registros (H,w)     -> Si el resultado es tipo word.
4. Los registros (U,E,H,w) -> Si el resultado es tipo dword.

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
* Los valores booleanos se cargan en "valBool"
* Los valores string se cargan en "valStr"

Las rutinas de operación, deben devolver su resultado en "res".
Para mayor información, consultar la doc. técnica.
 }
unit GenCod;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, SynEditHighlighter, Graphics, LCLType, LCLProc,
  SynFacilBasic, XpresTypesPIC, XPresParserPIC, XpresElementsPIC, GenCodPic,
  Pic16Utils, MisUtils, XpresBas;
type
    { TGenCod }
    TGenCod = class(TGenCodPic)
    protected
      procedure callParam(fun: TxpEleFun);
      procedure callFunct(fun: TxpEleFun);
    private  //Accesos a propeidades de p1^ y p2^.
      function bit1: TPicRegisterBit;
      function bit2: TPicRegisterBit;
      function byte1: TPicRegister;
      function byte2: TPicRegister;
      procedure ROB_byte_mod_byte(Opt: TxpOperation; SetRes: boolean);
      function valor1: word;
      function valor2: word;
    private  //Operaciones con Bit
//      f_byteXbyte_byte: TxpEleFun;  //índice para función
      f_byte_mul_byte_16: TxpEleFun;  //índice para función
      f_byte_div_byte: TxpEleFun;  //índice para función
      f_word_mul_word_16: TxpEleFun;  //índice para función
      procedure byte_div_byte(fun: TxpEleFun);
      procedure mul_byte_16(fun: TxpEleFun);
      procedure CopyInvert_C_to_Z;
      procedure fun_Byte(fun: TxpEleFun);
      procedure fun_DWord(fun: TxpEleFun);
      procedure ROB_bit_asig_bit(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_bit_asig_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_bit_and_bit(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_bit_and_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_bit_or_bit(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_bit_or_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_bit_xor_bit(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_bit_xor_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_bit_equ_bit(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_bit_equ_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_bit_dif_bit(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_bit_dif_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_div_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_mul_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROU_addr_word(Opr: TxpOperator; SetRes: boolean);
      procedure ROU_not_bit(Opr: TxpOperator; SetRes: boolean);
      procedure ROU_not_byte(Opr: TxpOperator; SetRes: boolean);
      procedure ROU_addr_byte(Opr: TxpOperator; SetRes: boolean);

      procedure ROB_word_and_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_word_umulword_word(Opt: TxpOperation; SetRes: boolean);
      procedure word_mul_word_16(fun: TxpEleFun);
    private  //Operaciones con boolean
      procedure ROB_bool_asig_bool(Opt: TxpOperation; SetRes: boolean);
      procedure ROU_not_bool(Opr: TxpOperator; SetRes: boolean);
      procedure ROB_bool_and_bool(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_bool_or_bool(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_bool_xor_bool(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_bool_equ_bool(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_bool_dif_bool(Opt: TxpOperation; SetRes: boolean);
    protected //Operaciones con byte
      procedure opers_byte(Opt: TxpOperation; const InstLW, InstWF: TPIC16Inst);
      procedure ROB_byte_asig_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_aadd_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_asub_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_sub_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_add_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_add_word(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_and_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_and_bit(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_or_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_or_bit(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_xor_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_xor_bit(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_equal_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_difer_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_difer_bit(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_great_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_less_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_gequ_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_lequ_byte(Opt: TxpOperation; SetRes: boolean);
      procedure CodifShift_by_W(aux: TPicRegister; toRight: boolean);
      procedure ROB_byte_shr_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_byte_shl_byte(Opt: TxpOperation; SetRes: boolean);
    private  //Operaciones con Word
      procedure ROB_word_asig_word(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_word_asig_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_word_equal_word(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_word_difer_word(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_word_great_word(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_word_add_word(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_word_add_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_word_sub_word(Opt: TxpOperation; SetRes: boolean);
    private  //Operaciones con DWord
      procedure ROB_dword_aadd_dword(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_dword_add_dword(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_dword_asig_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_dword_asig_dword(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_dword_asig_word(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_dword_difer_dword(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_dword_equal_dword(Opt: TxpOperation; SetRes: boolean);
    private  //Operaciones con Char
      procedure ROB_char_asig_char(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_char_equal_char(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_char_difer_char(Opt: TxpOperation; SetRes: boolean);
    protected //Operaciones con punteros
      procedure ROB_pointer_add_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROB_pointer_sub_byte(Opt: TxpOperation; SetRes: boolean);
      procedure ROU_derefPointer(Opr: TxpOperator; SetRes: boolean);
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
      procedure fun_Bool(fun: TxpEleFun);
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
      procedure DefPointerArithmetic(etyp: TxpEleType);
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
{Rutina genérica para llamar a una función definida por el usuario.}
begin
  fun.iniBnk := CurrBank;   //fija el banco inicial
  //Por ahora, no se implementa paginación, pero despuñes habría que considerarlo.
  _CALL(fun.adrr);  //codifica el salto
  //Verifica la optimizaicón de cambio de banco
  if OptBnkAftPro then begin
    //Se debe optimizar, fijando el banco que deja la función
    CurrBank := fun.ExitBank;
  end else begin
    //Se debe incluir siempre instrucciones de cambio de banco
    _BANKRESET;
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
  Result := (var1.addr0 = var2.addr0) and (var1.bit0 = var2.bit0);
end;
//Accesos a propeidades de p1^ y p2^.
function TGenCod.valor1: word; inline;
begin
  Result := p1^.valInt;
end;
function TGenCod.valor2: word; inline;
begin
  Result := p2^.valInt;
end;
function TGenCod.byte1: TPicRegister; inline;
begin
  Result := p1^.rVar.adrByte0;
end;
function TGenCod.byte2: TPicRegister; inline;
begin
  Result := p2^.rVar.adrByte0;
end;
function TGenCod.bit1: TPicRegisterBit; inline;
begin
  Result := p1^.rVar.adrBit;
end;
function TGenCod.bit2: TPicRegisterBit; inline;
begin
  Result := p2^.rVar.adrBit;
end;
////////////operaciones con Bit
procedure TGenCod.ROB_bit_asig_bit(Opt: TxpOperation; SetRes: boolean);
begin
  if p1^.Sto <> stVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.Sto of
  stConst : begin
    SetROBResultExpres_bit(Opt, false);  //Realmente, el resultado no es importante
    {Actualmente no existen constantes de tipo "Bit", ya que el número menor que se
    reconoce es de typo byte. Por eso se define ROB_bit_asig_byte(). }
    if p2^.valBool then begin
      kBSF(bit1);
    end else begin
      kBCF(bit1);
    end;
  end;
  stVariab: begin
    SetROBResultExpres_bit(Opt, false);  //Realmente, el resultado no es importante
    if IsTheSameBitVar(p1^.rVar, p2^.rVar) then begin
      //Es asignación de la misma variable.
      if p2^.Inverted then begin  //Es a := not a
          //verifica error.
        kMOVLW(p1^.rVar.BitMask);  //carga máscara
        kXORWF(byte1, toF);  //Se usa como byte
      end else begin  //Es a := a
        PutTopComm('No code, by optimizing.');
      end;
    end else begin
      //Es asignación de otra variable
      if p1^.rVar.bank = p2^.rVar.bank then begin //Están en el mismo banco
        //No se usa el registro W
        kBCF(bit1);
        if p2^.Inverted then kBTFSS(bit2) else kBTFSC(bit2);
        kBSF(bit1);
        //No hay problema con el banco final, porque es el mismo
      end else begin  //Están en bancos diferentes
        //No se usa el registro W
        kBCF(bit1);
        if p2^.Inverted then kBTFSS(bit2) else kBTFSC(bit2);
        kBSF(bit1);
        CurrBank := 255; //No se puede predecir el banco
      end;
    end;
  end;
  stExpres: begin  //ya está en STATUS.Z
    SetROBResultExpres_bit(Opt, false);  //Realmente, el resultado no es importante
    if p2^.Inverted then begin  //está invertido
      //No se usa el registro W
      kBCF(bit1);
      kBTFSS(Z);
      kBSF(bit1);
    end else begin  //caso normal
      //No se usa el registro W
      kBCF(bit1);
      kBTFSC(Z);
      kBSF(bit1);
    end;
  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
  end;
end;
procedure TGenCod.ROB_bit_asig_byte(Opt: TxpOperation; SetRes: boolean);
begin
  if p1^.Sto <> stVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.Sto of
  stConst : begin
    SetROBResultExpres_bit(Opt, false);  //Realmente, el resultado no es importante
    {Esta es la única opción válida, pero solo para los valores 0 y 1}
    if p2^.valInt = 0 then begin
      //No se usa el registro W
      kBCF(bit1);
    end else if p2^.valInt = 1 then begin
      //No se usa el registro W
      kBSF(bit1);
    end else begin
      GenError('Invalid value for a bit variable.'); exit;
    end;
  end;
  stVariab,
  stExpres: begin  //ya está en STATUS.Z
    GenError('Cannot asign: (bit) := (byte).'); exit;
  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
  end;
end;
procedure TGenCod.ROB_bit_and_bit(Opt: TxpOperation; SetRes: boolean);
begin
    case stoOperation of
    stConst_Const: begin  //AND de dos constantes. Caso especial
      SetROBResultConst_bit(p1^.valBool and p2^.valBool);
      exit;  //sale aquí, porque es un caso particular
    end;
    stConst_Variab: begin
      if p1^.valBool then begin  //p1 = 1
        //No usa ningún registro
        //Optimiza devolviendo la misma variable
        SetROBResultVariab(p2^.rVar, p2^.Inverted);  //mantiene la lógica
      end else begin   //p1 = 0
        //No usa ningún registro
        //Optimiza devolviendo constante = 0
        SetROBResultConst_bit(false);
      end;
    end;
    stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
      if p1^.valBool then begin  //p1 = 1
        //No usa ningún registro
        //Optimiza devolviendo la misma expresión en Z
        SetROBResultExpres_bit(Opt, p2^.Inverted);  //mantiene la lógica
      end else begin   //p1 = 0
        //No usa ningún registro
        //Optimiza devolviendo constante = 0
        SetROBResultConst_bit(false);
        Z.used := false;  //libera el bit Z, porque ya no importa la expresión
      end;
    end;
    stVariab_Const: begin
      if p2^.valBool then begin  //p2 = 1
        //No usa ningún registro
        //Optimiza devolviendo la misma variable
        SetROBResultVariab(p1^.rVar, p1^.Inverted);  //mantiene la lógica
      end else begin   //p2 = 0
        //No usa ningún registro
        //Optimiza devolviendo constante = 0
        SetROBResultConst_bit(false);
      end;
    end;
    stVariab_Variab:begin
      if IsTheSameBitVar(p1^.rVar, p2^.rVar) then begin
        //Es la misma variable: a AND a
        //Optimiza devolviendo la misma variable
        if p1^.Inverted and p2^.Inverted then begin
          //not a and not a = not a
          SetROBResultVariab(p1^.rVar, p1^.Inverted);
        end else if p1^.Inverted then begin
          //not a and a = 0
          SetROBResultConst_bit(false);
        end else if p2^.Inverted then begin
          //a and not a = 0
          SetROBResultConst_bit(false);
        end else begin  //Caso normal
          //a and a = a
          SetROBResultVariab(p1^.rVar, p1^.Inverted);
        end;
      end else begin
        if p1^.Inverted and p2^.Inverted then begin
          //Por La ley de Morgan, se convierten em OR
          p1^.Inverted := false;
          p2^.Inverted := false;
          ROB_bit_or_bit(Opt, SetRes);  //procesa como OR
          res.Invert;   //y niega todo
          exit;
        end else if p1^.Inverted then begin
          //Este caso es lo inverso, no vale la pena implementarlo de nuevo
          ExchangeP1_P2;
          ROB_bit_and_bit(Opt, SetRes);  //procesa como OR
          exit;
        end else if p2^.Inverted then begin
          SetROBResultExpres_bit(Opt, false);  //Fija resultado
          //Mueve p2 a Z
          kMOVLW(p2^.rVar.BitMask);
          kANDWF(byte2, toW);  //Z aparece normal
          //Aplica un AND entre Z y p1,
          kBTFSS(bit1);   //Si es 1, deja tal cual
          kBCF(Z);     //Si es 0, devuelve cero
        end else begin  //Caso normal
          SetROBResultExpres_bit(Opt, true);  //Fija resultado, con lógica invertida
          //Mueve p2 a Z
          kMOVLW(p2^.rVar.BitMask);
          kANDWF(byte2, toW);  //Z está invertido
          //Aplica un AND entre Z' y p1. Trabajamos con lógica invertida, por optimización
          kBTFSS(bit1); //Si es 1, deja tal cual (pero sigue con lógica invertida)
          kBSF(Z);       //Si es 0, devuelve cero (1 porque debe quedar con lógica invertida)
        end;
      end;
    end;
    stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
      if p1^.Inverted and p2^.Inverted then begin
        //Por La ley de Morgan, se convierten em OR
        p1^.Inverted := false;
        p2^.Inverted := false;
        ROB_bit_or_bit(Opt, SetRes);  //procesa como OR
        exit;
      end else if p1^.Inverted then begin  //lógica invertida en p1
        SetROBResultExpres_bit(Opt, false); //Fija resultado
        //Aplica un AND entre p1' y Z.
        kBTFSC(bit1); //Si es 0, deja tal cual
        kBCF(Z);      //Si es 1, devuelve cero
      end else if p2^.Inverted then begin  //lógica invertida en Z
        SetROBResultExpres_bit(Opt, true); //Deja la lógica invertida por optimización
        //Aplica un AND entre p1 y Z'.
        kBTFSS(bit1); //Si es 1, deja tal cual
        kBSF(Z);      //Si es 0, devuelve cero (1, porque es lógica es invertida)
      end else begin  //lógica normal
        SetROBResultExpres_bit(Opt, false); //Fija resultado
        //Aplica un AND entre p1 y Z.
        kBTFSS(bit1); //Si es 1, deja tal cual
        kBCF(Z);      //Si es 0, devuelve cero
      end;
    end;
    stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
      ExchangeP1_P2;       //Convierte en stConst_Expres
      ROB_bit_and_bit(Opt, SetRes);
      exit;
    end;
    stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
      ExchangeP1_P2;       //Convierte en stVariab_Expres
      ROB_bit_and_bit(Opt, SetRes);
      exit;
    end;
    stExpres_Expres:begin
      //la expresión p1 debe estar salvada y p2 en el acumulador
      p1^.SetAsVariab(GetVarBitFromStk);
      //Luego el caso es similar a variable-expresión
      ROB_bit_and_bit(Opt, SetRes);
      FreeStkRegisterBit;   //Libera pila. Ya se usó el dato.
    end;
    else
      genError('Cannot Compile: "%s"', [Opt.OperationString]);
    end;
end;
procedure TGenCod.ROB_bit_and_byte(Opt: TxpOperation; SetRes: boolean);
begin
  if p2^.Sto <> stConst then begin
    GenError('Incompatible types: (bit) AND (byte).'); exit;
  end;
  //p2 es constante
  if p2^.valInt = 0 then begin
    p2^.SetAsConst(typBit);   //convierte en bit
    p2^.valBool := false;
    ROB_bit_and_bit(Opt, SetRes);  //opera como bit
  end else if p2^.valInt = 1 then begin
    p2^.SetAsConst(typBit);   //convierte en bit
    p2^.valBool := true;
    ROB_bit_and_bit(Opt, SetRes);  //opera como bit
  end else begin
    GenError('Incompatible types: (bit) AND (byte).'); exit;
  end;
end;
procedure TGenCod.ROB_bit_or_bit(Opt: TxpOperation; SetRes: boolean);
begin
    case stoOperation of
    stConst_Const: begin  //AND de dos constantes. Caso especial
      SetROBResultConst_bit(p1^.valBool or p2^.valBool);
      exit;  //sale aquí, porque es un caso particular
    end;
    stConst_Variab: begin
      if p1^.valBool then begin  //p1 = 1
        //No usa ningún registro
        //Optimiza devolviendo constante = 1
        SetROBResultConst_bit(true);
      end else begin   //p1 = 0
        //No usa ningún registro
        //Optimiza devolviendo la misma variable
        SetROBResultVariab(p2^.rVar, p2^.Inverted);
      end;
    end;
    stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
      if p1^.valBool then begin  //p1 = 1
        //No usa ningún registro
        //Optimiza devolviendo constante = 1
        SetROBResultConst_bit(true);
        Z.used := false;  //libera el bit Z, porque ya no importa la expresión
      end else begin   //p1 = 0
        //No usa ningún registro
        //Optimiza devolviendo la misma expresión en Z
        SetROBResultExpres_bit(Opt, p2^.Inverted);  //mantiene la lógica
      end;
    end;
    stVariab_Const: begin
      if p2^.valBool then begin  //p2 = 1
        //No usa ningún registro
        //Optimiza devolviendo constante = 1
        SetROBResultConst_bit(true);
      end else begin   //p2 = 0
        //No usa ningún registro
        //Optimiza devolviendo la misma variable
        SetROBResultVariab(p1^.rVar, p1^.Inverted);
      end;
    end;
    stVariab_Variab:begin
      if IsTheSameBitVar(p1^.rVar, p2^.rVar) then begin
        //Es la misma variable: a OR a. Optimiza
        if p1^.Inverted and p2^.Inverted then begin
          //not a or not a = not a
          SetROBResultVariab(p1^.rVar, p1^.Inverted);
        end else if p1^.Inverted then begin
          //not a or a = 1
          SetROBResultConst_bit(true);
        end else if p2^.Inverted then begin
          //a or not a = 1
          SetROBResultConst_bit(true);
        end else begin  //Caso normal
          //a and a = a
          SetROBResultVariab(p1^.rVar, p1^.Inverted);
        end;
      end else begin
        if p1^.Inverted and p2^.Inverted then begin
          //Por La ley de Morgan, se convierten em AND
          p1^.Inverted := false;
          p2^.Inverted := false;
          ROB_bit_and_bit(Opt, SetRes);  //procesa como OR
          res.Invert;
          exit;
        end else if p1^.Inverted then begin
          //Este caso es lo inverso, no vale la pena implementarlo de nuevo
          ExchangeP1_P2;
          ROB_bit_or_bit(Opt, SetRes);  //procesa como OR
          exit;
        end else if p2^.Inverted then begin
          SetROBResultExpres_bit(Opt, false);  //Fija resultado
          //Mueve p2 a Z
          kMOVLW(p2^.rVar.BitMask);
          kANDWF(byte2, toW);  //Z aparece normal
          //Aplica un OR entre Z y p1,
          kBTFSC(bit1);   //Si es 0, deja tal cual
          kBSF(Z);     //Si es 1, devuelve uno
        end else begin  //Caso normal
          SetROBResultExpres_bit(Opt, true);  //Fija resultado, con lógica invertida
          //Mueve p2 a Z
          kMOVLW(p2^.rVar.BitMask);
          kANDWF(byte2, toW);  //Z está invertido
          //Aplica un OR entre p1 y Z'. Trabajamos con lógica invertida, por optimización
          kBTFSC(bit1); //Si es 0, deja tal cual (pero sigue con lógica invertida)
          kBCF(Z);       //Si es 1, devuelve 1 (0 porque debe quedar con lógica invertida)
        end;
      end;
    end;
    stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
      if p1^.Inverted and p2^.Inverted then begin
        //Por La ley de Morgan, se convierten em AND
        p1^.Inverted := false;
        p2^.Inverted := false;
        ROB_bit_and_bit(Opt, SetRes);  //procesa como OR
        exit;
      end else if p1^.Inverted then begin  //lógica invertida
        SetROBResultExpres_bit(Opt, false);  //Fija resultado
        //Aplica un OR entre p1' y Z.
        kBTFSS(bit1);   //Si es 1, deja tal cual
        kBSF(Z);     //Si es 0, devuelve uno
      end else if p2^.Inverted then begin  //lógica invertida en Z
        SetROBResultExpres_bit(Opt, true); //Deja la lógica invertida por optimización
        //Aplica un OR entre p1 y Z.
        kBTFSC(bit1);   //Si es 0, deja tal cual
        kBCF(Z);     //Si es 1, devuelve uno (0 porque es lógica invertida)
      end else begin   //lógica normal
        SetROBResultExpres_bit(Opt, false);  //Fija resultado
        //Aplica un OR entre p1 y Z.
        kBTFSC(bit1);   //Si es 0, deja tal cual
        kBSF(Z);     //Si es 1, devuelve uno
      end;
    end;
    stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
      ExchangeP1_P2;       //Convierte en stConst_Expres
      ROB_bit_or_bit(Opt, SetRes);
      exit;
    end;
    stExpres_Variab:begin  //la expresión p2 se evaluó y esta en W
      ExchangeP1_P2;       //Convierte en stVariab_Expres
      ROB_bit_or_bit(Opt, SetRes);
      exit;
    end;
    stExpres_Expres:begin
      //la expresión p1 debe estar salvada y p2 en el acumulador
      p1^.SetAsVariab(GetVarBitFromStk);
      //Luego el caso es similar a variable-expresión
      ROB_bit_or_bit(Opt, SetRes);
      FreeStkRegisterBit;   //Libera pila. Ya se usó el dato.
    end;
    else
      genError('Cannot Compile: "%s"', [Opt.OperationString]);
    end;
end;
procedure TGenCod.ROB_bit_or_byte(Opt: TxpOperation; SetRes: boolean);
begin
  if p2^.Sto <> stConst then begin
    GenError('Incompatible types: (bit) OR (byte).'); exit;
  end;
  //p2 es constante
  if p2^.valInt = 0 then begin
    p2^.SetAsConst(typBit);   //convierte en bit
    p2^.valBool := false;
    ROB_bit_or_bit(Opt, SetRes);  //opera como bit
  end else if p2^.valInt = 1 then begin
    p2^.SetAsConst(typBit);   //convierte en bit
    p2^.valBool := true;
    ROB_bit_or_bit(Opt, SetRes);  //opera como bit
  end else begin
    GenError('Incompatible types: (bit) OR (byte).'); exit;
  end;
end;
procedure TGenCod.ROB_bit_xor_bit(Opt: TxpOperation; SetRes: boolean);
begin
    case stoOperation of
    stConst_Const: begin  //XOR de dos constantes. Caso especial
      SetROBResultConst_bit(p1^.valBool xor p2^.valBool);
      exit;  //sale aquí, porque es un caso particular
    end;
    stConst_Variab: begin
      if p1^.valBool then begin  //p1 = 1
        //Optimiza devolviendo la variable invertida
        SetROBResultVariab(p2^.rVar, not p2^.Inverted);
      end else begin   //p1 = 0
        //Optimiza devolviendo la misma variable
        SetROBResultVariab(p2^.rVar, p2^.Inverted);
      end;
    end;
    stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
      if p1^.valBool then begin  //p1 = 1
        //Optimiza devolviendo la expresión invertida
        SetROBResultExpres_bit(Opt, not p2^.Inverted);  //mantiene la lógica
      end else begin   //p1 = 0
        //Optimiza devolviendo la misma expresión en Z
        SetROBResultExpres_bit(Opt, p2^.Inverted);  //mantiene la lógica
      end;
    end;
    stVariab_Const: begin
      ExchangeP1_P2;  //Convierte a stConst_Variab
      ROB_bit_xor_bit(Opt, SetRes);
      exit;
    end;
    stVariab_Variab:begin
      if IsTheSameBitVar(p1^.rVar, p2^.rVar) then begin
        //Es la misma variable: a XOR a
        //Optimiza devolviendo cero
        SetROBResultConst_bit(false);
      end else begin
        if p1^.Inverted and p2^.Inverted then begin
          p1^.Inverted := false;
          p2^.Inverted := false;
          ROB_bit_xor_bit(Opt, SetRes);  //es lo mismo
          exit;
        end else if p1^.Inverted then begin
          //Este caso es lo inverso, no vale la pena implementarlo de nuevo
          ExchangeP1_P2;
          ROB_bit_xor_bit(Opt, SetRes);  //procesa como OR
          exit;
        end else if p2^.Inverted then begin
          //a XOR b' = (z XOR b)'
          p2^.Inverted := false;
          ROB_bit_xor_bit(Opt, SetRes);
          res.Invert;  //Invierte la lógica
          exit;
        end else begin  //Caso normal
          {Se optimiza bien, esta operación, porque es una rutina muy usada para loa
          las operaciones XOR, y porque también se utiliza el XOR para las comparaciones
          de bits.}
          if p1^.bit = p2^.bit then begin
            //Están en el mismo bit, se puede optimizar
            SetROBResultExpres_bit(Opt, true);  //Fija resultado
            kMOVF(byte2, toW);  //mueve a W
            kXORWF(byte1, toW);      //APlica XOR,
            kANDLW(p1^.rVar.BitMask);  //Aplica máscara al bit que nos interesa, queda en Z, invertido
          end else if p1^.bit = p2^.bit +1 then begin
            //p1 está a un bit a la izquierda, se puede optimizar
            SetROBResultExpres_bit(Opt, true);  //Fija resultado
            kRLF(byte2, toW);  //alinea y mueve a W
            kXORWF(byte1, toW);      //APlica XOR,
            kANDLW(p1^.rVar.BitMask);  //Aplica máscara al bit que nos interesa, queda en Z, invertido
          end else if p1^.bit = p2^.bit-1 then begin
            //p1 está a un bit a la derecha, se puede optimizar
            SetROBResultExpres_bit(Opt, true);  //Fija resultado
            kRRF(byte2, toW);  //alinea y mueve a W
            kXORWF(byte1, toW);      //APlica XOR,
            kANDLW(p1^.rVar.BitMask);  //Aplica máscara al bit que nos interesa, queda en Z, invertido
          end else if abs(p1^.bit - p2^.bit) = 4 then begin
            //p1 está a un nibble de distancia, se puede optimizar
            SetROBResultExpres_bit(Opt, true);  //Fija resultado
            kSWAPF(byte2, toW);  //alinea y mueve a W
            kXORWF(byte1, toW);      //APlica XOR,
            kANDLW(p1^.rVar.BitMask);  //Aplica máscara al bit que nos interesa, queda en Z, invertido
          end else begin
            //La forma larga
            SetROBResultExpres_bit(Opt, false);  //Fija resultado,
            //Mueve p2 a Z
            kMOVLW(p2^.rVar.BitMask);
            kANDWF(byte2, toW);  //Z está invertido
            //Aplica un XOR entre p1 y Z'.
            _BANKSEL(p1^.bank);
            _MOVLW($1 << Z.bit);   //carga máscara, y deja lista si es que se necesita
            _BTFSS(p1^.offs, p1^.bit);  //Si es 1, invierte, pero ya esta invertido, así que lo deja
            _ANDWF(Z.offs, toW);  //Si es 0, deja tal cual, pero como está invertido, hay que corregir
          end;
        end;
      end;
    end;
    stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
      if p1^.Inverted and p2^.Inverted then begin
        p1^.Inverted := false;
        p2^.Inverted := false;
        ROB_bit_xor_bit(Opt, SetRes);   //es lo mismo
        exit;
      end else if p1^.Inverted then begin  //lógica invertida
        SetROBResultExpres_bit(Opt, false);  //Fija resultado
        //Aplica un XOR entre p1' y Z.
        _BANKSEL(p1^.bank);
        _MOVLW($1 << Z.bit);   //carga máscara, y deja lista si es eu se necesita
        _BTFSS(p1^.offs, p1^.bit);   //Si es 1(0), deja tal cual
        _ANDWF(Z.offs, toW);     //Si es 0(1), invierte
      end else if p2^.Inverted then begin  //lógica invertida en Z
        SetROBResultExpres_bit(Opt, false);  //Fija resultado
        //Aplica un XOR entre p1 y Z'.
        _BANKSEL(p1^.bank);
        _MOVLW($1 << Z.bit);   //carga máscara, y deja lista si es eu se necesita
        _BTFSS(p1^.offs, p1^.bit);   //Si es 1, invierte (deja igual porque ya está invertido)
        _ANDWF(Z.offs, toW);     //Si es 0, deja tal cual (realmente debe invertir)
      end else begin   //lógica normal
        SetROBResultExpres_bit(Opt, false);  //Fija resultado
        //Aplica un XOR entre p1 y Z.
        _BANKSEL(p1^.bank);
        _MOVLW($1 << Z.bit);   //carga máscara, y deja lista si es se necesita
        _BTFSC(p1^.offs, p1^.bit);  //Si es 0, deja tal cual
        _ANDWF(Z.offs, toW);         //Si es 1, invierte
      end;
    end;
    stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
      ExchangeP1_P2;       //Convierte en stConst_Expres
      ROB_bit_xor_bit(Opt, SetRes);
      exit;
    end;
    stExpres_Variab:begin  //la expresión p2 se evaluó y esta en W
      ExchangeP1_P2;       //Convierte en stVariab_Expres
      ROB_bit_xor_bit(Opt, SetRes);
      exit;
    end;
    stExpres_Expres:begin
      //la expresión p1 debe estar salvada y p2 en el acumulador
      p1^.SetAsVariab(GetVarBitFromStk);
      //Luego el caso es similar a stVariab_Expres
      ROB_bit_xor_bit(Opt, SetRes);
      FreeStkRegisterBit;   //Libera pila. Ya se usó el dato.
    end;
    else
      genError('Cannot Compile: "%s"', [Opt.OperationString]);
    end;
end;
procedure TGenCod.ROB_bit_xor_byte(Opt: TxpOperation; SetRes: boolean);
begin
  if p2^.Sto <> stConst then begin
    GenError('Incompatible types: (bit) XOR (byte).'); exit;
  end;
  //p2 es constante
  if p2^.valInt = 0 then begin
    p2^.SetAsConst(typBit);   //convierte en bit
    p2^.valBool := false;
    ROB_bit_xor_bit(Opt, SetRes);  //opera como bit
  end else if p2^.valInt = 1 then begin
    p2^.SetAsConst(typBit);   //convierte en bit
    p2^.valBool := true;
    ROB_bit_xor_bit(Opt, SetRes);  //opera como bit
  end else begin
    GenError('Incompatible types: (bit) XOR (byte).'); exit;
  end;
end;
procedure TGenCod.ROB_bit_equ_bit(Opt: TxpOperation; SetRes: boolean);
begin
  //Una comparación, es lo mismo que un XOR negado
  ROB_bit_xor_bit(Opt, SetRes);  //puede devolver error
  //Niega la lógica
  res.Invert;  //Invierte la lógica
  ChangeResultBitToBool;  //devuelve boolean
end;
procedure TGenCod.ROB_bit_equ_byte(Opt: TxpOperation; SetRes: boolean);
begin
  //Una comparación, es lo mismo que un XOR negado
  ROB_bit_xor_byte(Opt, SetRes);  //puede devolver error
  //¿Y si devuelve variable?
  res.Invert;  //Invierte la lógica
  ChangeResultBitToBool;  //devuelve boolean
end;
procedure TGenCod.ROB_bit_dif_bit(Opt: TxpOperation; SetRes: boolean);
begin
  //Esta comparación, es lo mismo que un XOR
  ROB_bit_xor_bit(Opt, SetRes);  //puede devolver error
  ChangeResultBitToBool;  //devuelve boolean
end;
procedure TGenCod.ROB_bit_dif_byte(Opt: TxpOperation; SetRes: boolean);
begin
  //Una comparación, es lo mismo que un XOR
  ROB_bit_xor_byte(Opt, SetRes);  //puede devolver error
  ChangeResultBitToBool;  //devuelve boolean
end;
procedure TGenCod.ROU_not_bit(Opr: TxpOperator; SetRes: boolean);
begin
  case p1^.Sto of
  stConst : begin
    {Actualmente no existen constantes de tipo "Bit", pero si existieran, sería así}
    SetROUResultConst_bit(not p1^.valBool);
  end;
  stVariab: begin
    {Optimiza devolviendo la misma variable, pero invirtiendo la lógica.}
    SetROBResultVariab(p1^.rVar, not p1^.Inverted);
  end;
  stExpres: begin  //ya está en STATUS.Z
    //No cambiamos su valor, sino su significado.
    SetROUResultExpres_bit(not p1^.Inverted);
  end;
  else
    genError('Not implemented: "%s"', [Opr.OperationString]);
  end;
end;
procedure TGenCod.ROU_not_byte(Opr: TxpOperator; SetRes: boolean);
begin
  case p1^.Sto of
  stConst : begin
    {Actualmente no existen constantes de tipo "Bit", pero si existieran, sería así}
    SetROUResultConst_byte((not p1^.valInt) and $FF);
  end;
  stVariab: begin
    SetROUResultExpres_byte;
    _COMF(p1^.offs, toW);
  end;
//  stExpres: begin
//    SetROUResultExpres_byte;
//    //////
//  end;
  else
    genError('Not implemented: "%s"', [Opr.OperationString]);
  end;
end;
procedure TGenCod.ROU_addr_byte(Opr: TxpOperator; SetRes: boolean);
{Devuelve la dirección de una variable.}
begin
  case p1^.Sto of
  stConst : begin
    genError('Cannot obtain address of constant.');
  end;
  stVariab: begin
    //Es una variable normal
    //La dirección de una variable es constante
    SetResultConst(typByte);
    //No se usa p1^.offs, porque solo retorna 7 bits;
    res.valInt := p1^.rVar.addr and $ff;
  end;
  stExpres: begin  //ya está en STATUS.Z
    genError('Cannot obtain address of an expression.');
  end;
  else
    genError('Cannot obtain address of this operand.');
  end;
end;
////////////operaciones con Boolean
procedure TGenCod.ROB_bool_asig_bool(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_bit_asig_bit(Opt, SetRes);  //A bajo nivel es lo mismo
end;
procedure TGenCod.ROU_not_bool(Opr: TxpOperator; SetRes: boolean);
begin
  ROU_not_bit(Opr, SetRes);  //A bajo nivel es lo mismo
  ChangeResultBitToBool;  //pero debe devolver este tipo
end;
procedure TGenCod.ROB_bool_and_bool(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_bit_and_bit(Opt, SetRes);  //A bajo nivel es lo mismo
  ChangeResultBitToBool;  //pero debe devolver este tipo
end;
procedure TGenCod.ROB_bool_or_bool(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_bit_or_bit(Opt, SetRes);  //A bajo nivel es lo mismo
  ChangeResultBitToBool;  //pero debe devolver este tipo
end;
procedure TGenCod.ROB_bool_xor_bool(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_bit_xor_bit(Opt, SetRes);  //A bajo nivel es lo mismo
  ChangeResultBitToBool;  //pero debe devolver este tipo
end;
procedure TGenCod.ROB_bool_equ_bool(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_bit_equ_bit(Opt, SetRes);  //Es lo mismo
end;
procedure TGenCod.ROB_bool_dif_bool(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_bit_dif_bit(Opt, SetRes);
end;
//////////// Operaciones con Byte
procedure TGenCod.ROB_byte_asig_byte(Opt: TxpOperation; SetRes: boolean);
var
  aux: TPicRegister;
  rVar: TxpEleVar;
begin
  //Simplifcamos el caso en que p2, sea de tipo p2^
  if not ChangePointerToExpres(p2^) then exit;
  //Realiza la asignación
  if p1^.Sto = stVariab then begin
    SetResultNull;  //Fomalmente,  una aisgnación no devuelve valores en Pascal
    //Asignación a una variable
    case p2^.Sto of
    stConst : begin
      if p2^.valInt=0 then begin
        //caso especial
        kCLRF(byte1);
      end else begin
        kMOVLW(valor2);
        kMOVWF(byte1);
      end;
    end;
    stVariab: begin
      kMOVF(byte2, toW);
      kMOVWF(byte1);
    end;
    stExpres: begin  //ya está en w
      kMOVWF(byte1);
    end;
    else
      GenError('No soportado'); exit;
    end;
  end else if p1^.Sto = stVarRefExp then begin
    {Este es un caso especial de asignación a un puntero a byte dereferenciado, pero
    cuando el valor del puntero es una expresión. Algo así como (ptr + 1)^}
    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
    case p2^.Sto of
    stConst : begin
      kMOVWF(FSR);  //direcciona
      //Asignación normal
      if p2^.valInt=0 then begin
        //caso especial
        kCLRF(INDF);
      end else begin
        kMOVLW(valor2);
        kMOVWF(INDF);
      end;
    end;
    stVariab: begin
      kMOVWF(FSR);  //direcciona
      //Asignación normal
      kMOVF(byte2, toW);
      kMOVWF(INDF);
    end;
    stExpres: begin
      //La dirección está en la pila y la expresión en W
      aux := GetAuxRegisterByte;
      kMOVWF(aux);   //Salva W (p2)
      //Apunta con p1
      rVar := GetVarByteFromStk;
      kMOVF(rVar.adrByte0, toW);  //Opera directamente al dato que había en la pila. Deja en W
      kMOVWF(FSR);  //direcciona
      //Asignación normal
      kMOVF(aux, toW);
      kMOVWF(INDF);
      aux.used := false;
      exit;
    end;
    else
      GenError('No soportado'); exit;
    end;
  end else if p1^.Sto = stVarRefVar then begin
    //Asignación a una variable
    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
    case p2^.Sto of
    stConst : begin
      //Caso especial de asignación a puntero desreferenciado: variable^
      kMOVF(byte1, toW);
      kMOVWF(FSR);  //direcciona
      //Asignación normal
      if p2^.valInt=0 then begin
        //caso especial
        kCLRF(INDF);
      end else begin
        kMOVLW(valor2);
        kMOVWF(INDF);
      end;
    end;
    stVariab: begin
      //Caso especial de asignación a puntero derefrrenciado: variable^
      kMOVF(byte1, toW);
      kMOVWF(FSR);  //direcciona
      //Asignación normal
      kMOVF(byte2, toW);
      kMOVWF(INDF);
    end;
    stExpres: begin  //ya está en w
      //Caso especial de asignación a puntero derefrrenciado: variable^
      aux := GetAuxRegisterByte;
      kMOVWF(aux);   //Salva W (p2)
      //Apunta con p1
      kMOVF(byte1, toW);
      kMOVWF(FSR);  //direcciona
      //Asignación normal
      kMOVF(aux, toW);
      kMOVWF(INDF);
      aux.used := false;
    end;
    else
      GenError('No soportado'); exit;
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure TGenCod.ROB_byte_aadd_byte(Opt: TxpOperation; SetRes: boolean);
var
  aux: TPicRegister;
  rVar: TxpEleVar;
begin
  //Simplifcamos el caso en que p2, sea de tipo p2^
  if not ChangePointerToExpres(p2^) then exit;
  //Caso especial de asignación
  if p1^.Sto = stVariab then begin
    SetResultNull;  //Fomalmente,  una aisgnación no devuelve valores en Pascal
    //Asignación a una variable
    case p2^.Sto of
    stConst : begin
      if p2^.valInt=0 then begin
        //Caso especial. No hace nada
      end else begin
        _MOVLW(p2^.valInt);
        _BANKSEL(p1^.bank);  //verifica banco destino
        _ADDWF(p1^.offs, toF);
      end;
    end;
    stVariab: begin
      _BANKSEL(p2^.bank);  //verifica banco fuente
      _MOVF(p2^.offs, toW);
      _BANKSEL(p1^.bank);  //verifica banco destino
      _ADDWF(p1^.offs, toF);
    end;
    stExpres: begin  //ya está en w
      _BANKSEL(p1^.bank);  //verifica banco destino
      _ADDWF(p1^.offs, toF);
    end;
    else
      GenError('No soportado'); exit;
    end;
  end else if p1^.Sto = stVarRefExp then begin
    {Este es un caso especial de asignación a un puntero a byte dereferenciado, pero
    cuando el valor del puntero es una expresión. Algo así como (ptr + 1)^}
    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
    case p2^.Sto of
    stConst : begin
      //Asignación normal
      if p2^.valInt=0 then begin
        //Caso especial. No hace nada
      end else begin
        _MOVWF(FSR.offs);  //direcciona
        _MOVLW(p2^.valInt);
        _ADDWF(0, toF);
      end;
    end;
    stVariab: begin
      _MOVWF(FSR.offs);  //direcciona
      //Asignación normal
      _BANKSEL(p2^.bank);  //verifica banco fuente
      _MOVF(p2^.offs, toW);
      _ADDWF(0, toF);
    end;
    stExpres: begin
      //La dirección está en la pila y la expresión en W
      aux := GetAuxRegisterByte;
      _BANKSEL(aux.bank);
      _MOVWF(aux.offs);   //Salva W (p2)
      //Apunta con p1
      rVar := GetVarByteFromStk;
      _BANKSEL(rVar.adrByte0.bank);
      _MOVF(rVar.adrByte0.offs, toW);  //opera directamente al dato que había en la pila. Deja en W
      _MOVWF(FSR.offs);  //direcciona
      //Asignación normal
      _BANKSEL(aux.bank);  //verifica banco fuente
      _MOVF(aux.offs, toW);
      _ADDWF(0, toF);
      aux.used := false;
      exit;
    end;
    else
      GenError('No soportado'); exit;
    end;
  end else if p1^.Sto = stVarRefVar then begin
    //Asignación a una variable
    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
    case p2^.Sto of
    stConst : begin
      //Asignación normal
      if p2^.valInt=0 then begin
        //Caso especial. No hace nada
      end else begin
        //Caso especial de asignación a puntero dereferenciado: variable^
        _BANKSEL(p1^.bank);  //verifica banco destino
        _MOVF(p1^.offs, toW);
        _MOVWF(FSR.offs);  //direcciona
        _MOVLW(p2^.valInt);
        _ADDWF(0, toF);
      end;
    end;
    stVariab: begin
      //Caso especial de asignación a puntero derefrrenciado: variable^
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVF(p1^.offs, toW);
      _MOVWF(FSR.offs);  //direcciona
      //Asignación normal
      _BANKSEL(p2^.bank);  //verifica banco fuente
      _MOVF(p2^.offs, toW);
      _ADDWF(0, toF);
    end;
    stExpres: begin  //ya está en w
      //Caso especial de asignación a puntero derefrrenciado: variable^
      aux := GetAuxRegisterByte;
      _BANKSEL(aux.bank);
      _MOVWF(aux.offs);   //Salva W (p2)
      //Apunta con p1
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVF(p1^.offs, toW);
      _MOVWF(FSR.offs);  //direcciona
      //Asignación normal
      _BANKSEL(aux.bank);  //Salva W (p2)
      _MOVF(aux.offs, toW);
      _ADDWF(0, toF);
      aux.used := false;
    end;
    else
      GenError('No soportado'); exit;
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure TGenCod.ROB_byte_asub_byte(Opt: TxpOperation; SetRes: boolean);
var
  aux: TPicRegister;
  rVar: TxpEleVar;
begin
  //Simplifcamos el caso en que p2, sea de tipo p2^
  if not ChangePointerToExpres(p2^) then exit;
  //Caso especial de asignación
  if p1^.Sto = stVariab then begin
    SetResultNull;  //Fomalmente,  una aisgnación no devuelve valores en Pascal
    //Asignación a una variable
    case p2^.Sto of
    stConst : begin
      if p2^.valInt=0 then begin
        //Caso especial. No hace nada
      end else begin
        _MOVLW(p2^.valInt);
        _BANKSEL(p1^.bank);  //verifica banco destino
        _SUBWF(p1^.offs, toF);
      end;
    end;
    stVariab: begin
      _BANKSEL(p2^.bank);  //verifica banco fuente
      _MOVF(p2^.offs, toW);
      _BANKSEL(p1^.bank);  //verifica banco destino
      _SUBWF(p1^.offs, toF);
    end;
    stExpres: begin  //ya está en w
      _BANKSEL(p1^.bank);  //verifica banco destino
      _SUBWF(p1^.offs, toF);
    end;
    else
      GenError('No soportado'); exit;
    end;
  end else if p1^.Sto = stVarRefExp then begin
    {Este es un caso especial de asignación a un puntero a byte dereferenciado, pero
    cuando el valor del puntero es una expresión. Algo así como (ptr + 1)^}
    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
    case p2^.Sto of
    stConst : begin
      //Asignación normal
      if p2^.valInt=0 then begin
        //Caso especial. No hace nada
      end else begin
        _MOVWF(FSR.offs);  //direcciona
        _MOVLW(p2^.valInt);
        _SUBWF(0, toF);
      end;
    end;
    stVariab: begin
      _MOVWF(FSR.offs);  //direcciona
      //Asignación normal
      _BANKSEL(p2^.bank);  //verifica banco fuente
      _MOVF(p2^.offs, toW);
      _SUBWF(0, toF);
    end;
    stExpres: begin
      //La dirección está en la pila y la expresión en W
      aux := GetAuxRegisterByte;
      _BANKSEL(aux.bank);
      _MOVWF(aux.offs);   //Salva W (p2)
      //Apunta con p1
      rVar := GetVarByteFromStk;
      _BANKSEL(rVar.adrByte0.bank);
      _MOVF(rVar.adrByte0.offs, toW);  //opera directamente al dato que había en la pila. Deja en W
      _MOVWF(FSR.offs);  //direcciona
      //Asignación normal
      _BANKSEL(aux.bank);  //verifica banco fuente
      _MOVF(aux.offs, toW);
      _SUBWF(0, toF);
      aux.used := false;
      exit;
    end;
    else
      GenError('No soportado'); exit;
    end;
  end else if p1^.Sto = stVarRefVar then begin
    //Asignación a una variable
    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
    case p2^.Sto of
    stConst : begin
      //Asignación normal
      if p2^.valInt=0 then begin
        //Caso especial. No hace nada
      end else begin
        //Caso especial de asignación a puntero dereferenciado: variable^
        _BANKSEL(p1^.bank);  //verifica banco destino
        _MOVF(p1^.offs, toW);
        _MOVWF(FSR.offs);  //direcciona
        _MOVLW(p2^.valInt);
        _SUBWF(0, toF);
      end;
    end;
    stVariab: begin
      //Caso especial de asignación a puntero derefrrenciado: variable^
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVF(p1^.offs, toW);
      _MOVWF(FSR.offs);  //direcciona
      //Asignación normal
      _BANKSEL(p2^.bank);  //verifica banco fuente
      _MOVF(p2^.offs, toW);
      _SUBWF(0, toF);
    end;
    stExpres: begin  //ya está en w
      //Caso especial de asignación a puntero derefrrenciado: variable^
      aux := GetAuxRegisterByte;
      _BANKSEL(aux.bank);
      _MOVWF(aux.offs);   //Salva W (p2)
      //Apunta con p1
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVF(p1^.offs, toW);
      _MOVWF(FSR.offs);  //direcciona
      //Asignación normal
      _BANKSEL(aux.bank);  //Salva W (p2)
      _MOVF(aux.offs, toW);
      _SUBWF(0, toF);
      aux.used := false;
    end;
    else
      GenError('No soportado'); exit;
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure TGenCod.opers_byte(Opt: TxpOperation; const InstLW, InstWF:TPIC16Inst);
{Rutina general en operaciones con bytes}
var
  rVar: TxpEleVar;
begin
  case stoOperation of
  stConst_Variab: begin
    SetROBResultExpres_byte(Opt);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    CodAsmK(InstLW, p1^.valInt);  //deja en W
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    CodAsmK(InstLW, p1^.valInt);  //deja en W
  end;
  stVariab_Const: begin
    SetROBResultExpres_byte(Opt);
    _MOVLW(p2^.valInt);
    _BANKSEL(p1^.bank);
    CodAsmFD(InstWF, p1^.offs, toW);  //deja en W
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _BANKSEL(p1^.bank);
    CodAsmFD(InstWF, p1^.offs, toW);  //deja en W
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    _BANKSEL(p1^.bank);
    CodAsmFD(InstWF, p1^.offs, toW);  //deja en W
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    CodAsmK(InstLW, p2^.valInt);  //deja en W
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    _BANKSEL(p2^.bank);
    CodAsmFD(InstWF, p2^.offs, toW);  //deja en W
  end;
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
    //La expresión p1 debe estar salvada y p2 en el acumulador
    rVar := GetVarByteFromStk;
    _BANKSEL(rVar.adrByte0.bank);
    CodAsmFD(InstWF, rVar.adrByte0.offs, toW);  //opera directamente al dato que había en la pila. Deja en W
    FreeStkRegisterByte;   //libera pila porque ya se uso
  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
  end;
end;
procedure TGenCod.ROB_byte_add_byte(Opt: TxpOperation; SetRes: boolean);
var
  rVar: TxpEleVar;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  case stoOperation of
  stConst_Const: begin
    SetROBResultConst_byte(p1^.valInt+p2^.valInt);  //puede generar error
  end;
  stConst_Variab: begin
    if p1^.valInt = 0 then begin
      //Caso especial
      SetROBResultVariab(p2^.rVar);  //devuelve la misma variable
      exit;
    end else if p1^.valInt = 1 then begin
      //Caso especial
      SetROBResultExpres_byte(Opt);
      _BANKSEL(p2^.bank);
      _INCF(p2^.offs, toW);
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _ADDLW(p1^.valInt);  //deja en W
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    CodAsmK(ADDLW, p1^.valInt);  //deja en W
  end;
  stVariab_Const: begin
    ExchangeP1_P2;
    ROB_byte_add_byte(Opt, true);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _BANKSEL(p1^.bank);
    _ADDWF(p1^.offs, toW);  //deja en W
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    _BANKSEL(p1^.bank);
    _ADDWF(p1^.offs, toW);  //deja en W
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    CodAsmK(ADDLW, p2^.valInt);  //deja en W
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    _BANKSEL(p2^.bank);
    _ADDWF(p2^.offs, toW);  //deja en W
  end;
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
    //La expresión p1 debe estar salvada y p2 en el acumulador
    rVar := GetVarByteFromStk;
    _BANKSEL(rVar.adrByte0.bank);
    _ADDWF(rVar.adrByte0.offs, toW);  //opera directamente al dato que había en la pila. Deja en W
    FreeStkRegisterByte;   //libera pila porque ya se uso
  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
  end;
end;
procedure TGenCod.ROB_byte_add_word(Opt: TxpOperation; SetRes: boolean);
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  case stoOperation of
  stExpres_Expres:begin
    {Este es el único caso que no se puede invertir, por la posición de los operandos en
     la pila.}
    //la expresión p1 debe estar salvada y p2 en el acumulador
    p1^.SetAsVariab(GetVarByteFromStk);  //Convierte a variable
    //Luego el caso es similar a stVariab_Expres
    ROB_byte_add_word(Opt, SetRes);
    FreeStkRegisterByte;   //libera pila porque ya se usó el dato ahí contenido
  end;
  else
    //Para los otros casos, funciona
    ExchangeP1_P2;   //Invierte los operandos
    ROB_word_add_byte(Opt, SetRes); //Y llama a la función opuesta
  end;
end;
procedure TGenCod.ROB_byte_sub_byte(Opt: TxpOperation; SetRes: boolean);
var
  rVar: TxpEleVar;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  case stoOperation of
  stConst_Const:begin  //suma de dos constantes. Caso especial
    SetROBResultConst_byte(p1^.valInt-p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  stConst_Variab: begin
    SetROBResultExpres_byte(Opt);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _SUBLW(p1^.valInt);   //K - W -> W
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    _SUBLW(p1^.valInt);   //K - W -> W
  end;
  stVariab_Const: begin
    SetROBResultExpres_byte(Opt);
    _MOVLW(p2^.valInt);
    _BANKSEL(p1^.bank);
    _SUBWF(p1^.offs, toW);  //F - W -> W
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _BANKSEL(p1^.bank);
    _SUBWF(p1^.offs, toW);  //F - W -> W
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    _BANKSEL(p1^.bank);
    _SUBWF(p1^.offs, toW);  //F - W -> W
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    _SUBLW(p2^.valInt);  //K - W -> W
    _SUBLW(0);  //K - W -> W   //invierte W
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    _BANKSEL(p2^.bank);
    _SUBWF(p2^.offs, toW);  //F - W -> W
    _SUBLW(0);  //K - W -> W   //invierte W
  end;
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
    //la expresión p1 debe estar salvada y p2 en el acumulador
    rVar := GetVarByteFromStk;
    _BANKSEL(rVar.adrByte0.bank);
    _SUBWF(rVar.adrByte0.offs, toW);  //opera directamente al dato que había en la pila. Deja en W
    FreeStkRegisterByte;   //libera pila porque ya se uso
  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
  end;
end;
procedure TGenCod.mul_byte_16(fun: TxpEleFun);
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
procedure TGenCod.ROB_byte_mul_byte(Opt: TxpOperation; SetRes: boolean);
var
  rVar: TxpEleVar;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  case stoOperation of
  stConst_Const:begin  //producto de dos constantes. Caso especial
    SetROBResultConst_word((p1^.valInt*p2^.valInt) and $FFFF);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  stConst_Variab: begin
    if p1^.valInt=0 then begin  //caso especial
      SetROBResultConst_byte(0);
      exit;
    end else if p1^.valInt=1 then begin  //caso especial
      SetROBResultVariab(p2^.rVar);
      exit;
    end else if p1^.valInt=2 then begin
      SetROBResultExpres_word(Opt);
      _BANKSEL(H.bank);
      _CLRF(H.offs);
      _BCF(STATUS, _C);
      _BANKSEL(P2^.bank);
      _RLF(p2^.offs, toW);
      _BANKSEL(H.bank);
      _RLF(H.offs, toF);
      exit;
    end;
    SetROBResultExpres_word(Opt);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _BANKSEL(E.bank);
    _MOVWF(E.offs);
    _MOVLW(p1^.valInt);
    _CALL(f_byte_mul_byte_16.adrr);
    AddCallerTo(f_byte_mul_byte_16);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_word(opt);
    _BANKSEL(E.bank);
    _MOVWF(E.offs);
    _MOVLW(p1^.valInt);
    _CALL(f_byte_mul_byte_16.adrr);
    AddCallerTo(f_byte_mul_byte_16);
  end;
  stVariab_Const: begin
    SetROBResultExpres_word(opt);
    _BANKSEL(p1^.bank);
    _MOVF(p1^.offs, toW);
    _BANKSEL(E.bank);
    _MOVWF(E.offs);
    _MOVLW(p2^.valInt);
    _CALL(f_byte_mul_byte_16.adrr);
    AddCallerTo(f_byte_mul_byte_16);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_word(Opt);
    _BANKSEL(p1^.bank);
    _MOVF(p1^.offs, toW);
    _BANKSEL(E.bank);
    _MOVWF(E.offs);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _CALL(f_byte_mul_byte_16.adrr);
    AddCallerTo(f_byte_mul_byte_16);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_word(Opt);
    _BANKSEL(E.bank);
    _MOVWF(E.offs);  //p2 -> E
    _BANKSEL(p1^.bank);
    _MOVF(p1^.offs, toW); //p1 -> W
    _CALL(f_byte_mul_byte_16.adrr);
    AddCallerTo(f_byte_mul_byte_16);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_word(Opt);
    _BANKSEL(E.bank);
    _MOVWF(E.offs);  //p1 -> E
    _MOVLW(p2^.valInt); //p2 -> W
    _CALL(f_byte_mul_byte_16.adrr);
    AddCallerTo(f_byte_mul_byte_16);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_word(Opt);
    _BANKSEL(E.bank);
    _MOVWF(E.offs);  //p1 -> E
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW); //p2 -> W
    _CALL(f_byte_mul_byte_16.adrr);
    AddCallerTo(f_byte_mul_byte_16);
  end;
  stExpres_Expres:begin
    SetROBResultExpres_word(Opt);
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
    AddCallerTo(f_byte_mul_byte_16);
  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
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
procedure TGenCod.ROB_byte_div_byte(Opt: TxpOperation; SetRes: boolean);
var
  rVar: TxpEleVar;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  case stoOperation of
  stConst_Const:begin  //producto de dos constantes. Caso especial
    if p2^.valInt = 0 then begin
      GenError('Cannot divide by zero');
      exit;
    end;
    SetROBResultConst_byte(p1^.valInt div p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  stConst_Variab: begin
    if p1^.valInt=0 then begin  //caso especial
      SetROBResultConst_byte(0);
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    _MOVLW(p1^.valInt);
    _BANKSEL(H.bank);
    _MOVWF(H.offs);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _CALL(f_byte_div_byte.adrr);
    AddCallerTo(f_byte_div_byte);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    if p1^.valInt=0 then begin  //caso especial
      SetROBResultConst_byte(0);
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    _BANKSEL(E.bank);
    _MOVWF(E.offs);  //guarda divisor

    _MOVLW(p1^.valInt);
    _BANKSEL(H.bank);
    _MOVWF(H.offs);  //dividendo

    _BANKSEL(E.bank);
    _MOVF(E.offs, toW);  //divisor
    _CALL(f_byte_div_byte.adrr);
    AddCallerTo(f_byte_div_byte);
  end;
  stVariab_Const: begin
    if p2^.valInt = 0 then begin
      GenError('Cannot divide by zero');
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    _BANKSEL(p1^.bank);
    _MOVF(p1^.offs, toW);
    _BANKSEL(H.bank);
    _MOVWF(H.offs);
    _MOVLW(p2^.valInt);
    _CALL(f_byte_div_byte.adrr);
    AddCallerTo(f_byte_div_byte);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);
    _BANKSEL(p1^.bank);
    _MOVF(p1^.offs, toW);
    _BANKSEL(H.bank);
    _MOVWF(H.offs);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _CALL(f_byte_div_byte.adrr);
    AddCallerTo(f_byte_div_byte);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
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
    AddCallerTo(f_byte_div_byte);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    if p2^.valInt = 0 then begin
      GenError('Cannot divide by zero');
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    _BANKSEL(H.bank);
    _MOVWF(H.offs);  //p1 -> H
    _MOVLW(p2^.valInt); //p2 -> W
    _CALL(f_byte_div_byte.adrr);
    AddCallerTo(f_byte_div_byte);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    _BANKSEL(H.bank);
    _MOVWF(H.offs);  //p1 -> H
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW); //p2 -> W
    _CALL(f_byte_div_byte.adrr);
    AddCallerTo(f_byte_div_byte);
  end;
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
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
    AddCallerTo(f_byte_div_byte);
  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
  end;
end;
procedure TGenCod.ROB_byte_mod_byte(Opt: TxpOperation; SetRes: boolean);
var
  rVar: TxpEleVar;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  case stoOperation of
  stConst_Const : begin  //producto de dos constantes. Caso especial
    if p2^.valInt = 0 then begin
      GenError('Cannot divide by zero');
      exit;
    end;
    SetROBResultConst_byte(p1^.valInt mod p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  stConst_Variab: begin
    if p1^.valInt=0 then begin  //caso especial
      SetROBResultConst_byte(0);
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    kMOVLW(p1^.valInt);
    kMOVWF(H);
    kMOVF(byte2, toW);
    _CALL(f_byte_div_byte.adrr);
    //¿Y el banco de salida?
    kMOVF(U, toW);  //Resultado en W
    AddCallerTo(f_byte_div_byte);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    if p1^.valInt=0 then begin  //caso especial
      SetROBResultConst_byte(0);
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    kMOVWF(E);  //guarda divisor
    kMOVLW(p1^.valInt);
    kMOVWF(H);  //dividendo

    kMOVF(E, toW);  //divisor
    _CALL(f_byte_div_byte.adrr);
    kMOVF(U, toW);  //Resultado en W
    AddCallerTo(f_byte_div_byte);
  end;
  stVariab_Const: begin
    if p2^.valInt = 0 then begin
      GenError('Cannot divide by zero');
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    kMOVF(byte1, toW);
    kMOVWF(H);
    kMOVLW(p2^.valInt);
    _CALL(f_byte_div_byte.adrr);
    kMOVF(U, toW);  //Resultado en W
    AddCallerTo(f_byte_div_byte);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);
    kMOVF(byte1, toW);
    kMOVWF(H);
    kMOVF(byte2, toW);
    _CALL(f_byte_div_byte.adrr);
    kMOVF(U, toW);  //Resultado en W
    AddCallerTo(f_byte_div_byte);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    //guarda divisor
    kMOVWF(E);
    //p1 -> H
    kMOVF(byte1, toW); //p1 -> W
    kMOVWF(H);  //dividendo

    kMOVF(E, toW);  //divisor
    _CALL(f_byte_div_byte.adrr);
    kMOVF(U, toW);  //Resultado en W
    AddCallerTo(f_byte_div_byte);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    if p2^.valInt = 0 then begin
      GenError('Cannot divide by zero');
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    kMOVWF(H);  //p1 -> H
    kMOVLW(p2^.valInt); //p2 -> W
    _CALL(f_byte_div_byte.adrr);
    kMOVF(U, toW);  //Resultado en W
    AddCallerTo(f_byte_div_byte);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    kMOVWF(H);  //p1 -> H
    kMOVF(byte2, toW); //p2 -> W
    _CALL(f_byte_div_byte.adrr);
    kMOVF(U, toW);  //Resultado en W
    AddCallerTo(f_byte_div_byte);
  end;
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
    //la expresión p1 debe estar salvada y p2 en el acumulador
    rVar := GetVarByteFromStk;
    //guarda divisor
    kMOVWF(E);
    //pila -> H
    kMOVF(rVar.adrByte0, toW); //p1 -> W
    kMOVWF(H);  //dividendo
    //divisor -> W
    kMOVF(E, toW);  //p2 -> E
    _CALL(f_byte_div_byte.adrr);
    kMOVF(U, toW);  //Resultado en W
    FreeStkRegisterByte;   //libera pila porque se usará el dato ahí contenido
    {Se podría ahorrar el paso de mover la variable de la pila a W (y luego a una
    variable) temporal, si se tuviera una rutina de multiplicación que compilara a
    partir de la direccion de una variable (en este caso de la pila, que se puede
    modificar), pero es un caso puntual, y podría no reutilizar el código apropiadamente.}
    AddCallerTo(f_byte_div_byte);
  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
  end;
end;
procedure TGenCod.ROB_byte_and_byte(Opt: TxpOperation; SetRes: boolean);
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  if stoOperation  = stConst_Const then begin  //suma de dos constantes. Caso especial
    SetROBResultConst_byte(p1^.valInt and p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    opers_byte(Opt, ANDLW, ANDWF);
end;
procedure TGenCod.ROB_byte_and_bit(Opt: TxpOperation; SetRes: boolean);
begin
  {No hay problema en usar siempre ExchangeP1_P2, porque el caso Expresión-Expresión,
  no se implementa ROB_bit_and_byte.}
  ExchangeP1_P2;   //Invierte los operandos
  ROB_bit_and_byte(Opt, SetRes);
end;
procedure TGenCod.ROB_byte_or_byte(Opt: TxpOperation; SetRes: boolean);
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  if stoOperation  = stConst_Const then begin  //suma de dos constantes. Caso especial
    SetROBResultConst_byte(p1^.valInt or p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    opers_byte(Opt, IORLW, IORWF);
end;
procedure TGenCod.ROB_byte_or_bit(Opt: TxpOperation; SetRes: boolean);
begin
  {No hay problema en usar siempre ExchangeP1_P2, porque el caso Expresión-Expresión,
  no se implementa ROB_bit_or_byte.}
  ExchangeP1_P2;   //Invierte los operandos
  ROB_bit_or_byte(Opt, SetRes);
end;
procedure TGenCod.ROB_byte_xor_byte(Opt: TxpOperation; SetRes: boolean);
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  if stoOperation  = stConst_Const then begin  //suma de dos constantes. Caso especial
    SetROBResultConst_byte(p1^.valInt xor p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    opers_byte(Opt, XORLW, XORWF);
end;
procedure TGenCod.ROB_byte_xor_bit(Opt: TxpOperation; SetRes: boolean);
begin
  {No hay problema en usar siempre ExchangeP1_P2, porque el caso Expresión-Expresión,
  no se implementa ROB_bit_xor_byte.}
  ExchangeP1_P2;   //Invierte los operandos
  ROB_bit_xor_byte(Opt, SetRes);
end;
procedure TGenCod.ROB_byte_equal_byte(Opt: TxpOperation; SetRes: boolean);
var
  rVar: TxpEleVar;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  case stoOperation of
  stConst_Const: begin  //compara constantes. Caso especial
    SetROBResultConst_bool(p1^.valInt = p2^.valInt);
  end;
  stConst_Variab: begin
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
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
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    _SUBLW(p1^.valInt);  //si iguales _Z=1
  end;
  stVariab_Const: begin
    ExchangeP1_P2;  //Convierte a stConst_Variab
    ROB_byte_equal_byte(Opt, SetRes);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.offs, toW);
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.offs, toW);  //si iguales _Z=1
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    //ReserveW; if HayError then exit;
    _BANKSEL(p1^.bank);  //verifica banco destino
    _SUBWF(p1^.offs, toW);  //si iguales _Z=1
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    //ReserveW; if HayError then exit;
    _SUBLW(p2^.valInt);  //si iguales _Z=1
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    //ReserveW; if HayError then exit;
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.offs, toW);  //si iguales _Z=1
  end;
  stExpres_Expres:begin
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    //la expresión p1 debe estar salvada y p2 en el acumulador
    rVar := GetVarByteFromStk;
    _BANKSEL(rVar.adrByte0.bank);  //verifica banco destino
    _SUBWF(rVar.adrByte0.offs, toW);  //compara directamente a lo que había en pila.
    FreeStkRegisterByte;   //libera pila porque se usará el dato ahí contenido
  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
  end;
end;
procedure TGenCod.ROB_byte_difer_byte(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_byte_equal_byte(Opt, SetRes);  //usa el mismo código
  res.Invert;  //Invierte la lógica
end;
procedure TGenCod.ROB_byte_difer_bit(Opt: TxpOperation; SetRes: boolean);
begin
  {No hay problema en usar siempre ExchangeP1_P2, porque el caso Expresión-Expresión,
  no se implementa ROB_bit_dif_byte.}
  ExchangeP1_P2;
  ROB_bit_dif_byte(Opt, SetRes);
end;
procedure TGenCod.ROB_byte_great_byte(Opt: TxpOperation; SetRes: boolean);
var
  tmp: TPicRegister;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  case stoOperation of
  stConst_Const: begin  //compara constantes. Caso especial
    SetROBResultConst_bool(p1^.valInt > p2^.valInt);
  end;
  stConst_Variab: begin
    if p1^.valInt = 0 then begin
      //0 es mayor que nada
      SetROBResultConst_bool(false);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
      _MOVLW(p1^.valInt);
      _BANKSEL(p2^.bank);  //verifica banco destino
      _SUBWF(p2^.offs, toW);  //Si p1 > p2: C=0.
      CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
    end;
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    if p1^.valInt = 0 then begin
      //0 es mayor que nada
      SetROBResultConst_bool(false);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      //Optimiza rutina, usando: A>B  equiv. NOT (B<=A-1)
      //Se necesita asegurar que p1, es mayo que cero.
      SetROBResultExpres_bool(Opt, true);  //invierte la lógica
      //p2, ya está en W
      _SUBLW(p1^.valInt-1);  //Si p1 > p2: C=0.
      CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
    end;
  end;
  stVariab_Const: begin
    if p2^.valInt = 255 then begin
      //Nada es mayor que 255
      SetROBResultConst_bool(false);
      GenWarn('Expression will always be FALSE or TRUE.');
    end else begin
      SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVF(p1^.offs, toW);
      _SUBLW(p2^.valInt);  //Si p1 > p2: C=0.
      CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
    end;
  end;
  stVariab_Variab:begin
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.offs, toW);
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.offs, toW);  //Si p1 > p2: C=0.
    CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    tmp := GetAuxRegisterByte;  //Se pide registro auxiliar
    _MOVWF(tmp.offs);    //guarda resultado de expresión
    //Ahora es como stVariab_Variab
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.offs, toW);
    _BANKSEL(tmp.bank);  //verifica banco destino
    _SUBWF(tmp.offs, toW);  //Si p1 > tmp: C=0.
    CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
    tmp.used := false;  //libera
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    if p2^.valInt = 255 then begin
      //nada es mayor que 255
      SetROBResultConst_bool(false);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
  //    p1, ya está en W
      _SUBLW(p2^.valInt);  //Si p1 > p2: C=0.
      CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
    end;
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.offs, toW);  //Si p1 > p2: C=0.
    CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
  end;
  stExpres_Expres:begin
    //la expresión p1 debe estar salvada y p2 en el acumulador
    p1^.SetAsVariab(GetVarByteFromStk);  //Convierte a variable
    //Luego el caso es similar a stVariab_Expres
    ROB_byte_great_byte(Opt, true);
    FreeStkRegisterByte;   //libera pila porque ya se usó el dato ahí contenido
  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
  end;
end;
procedure TGenCod.ROB_byte_less_byte(Opt: TxpOperation; SetRes: boolean);
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  //A < B es lo mismo que B > A
  case stoOperation of
  stExpres_Expres:begin
    {Este es el único caso que no se puede invertir, por la posición de los operandos en
     la pila.}
    //la expresión p1 debe estar salvada y p2 en el acumulador
    p1^.SetAsVariab(GetVarByteFromStk);  //Convierte a variable
    //Luego el caso es similar a stVariab_Expres
    ROB_byte_less_byte(Opt, SetRes);
    FreeStkRegisterByte;   //libera pila porque ya se usó el dato ahí contenido
  end;
  else
    //Para los otros casos, funciona
    ExchangeP1_P2;
    ROB_byte_great_byte(Opt, SetRes);
  end;
end;
procedure TGenCod.ROB_byte_gequ_byte(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_byte_less_byte(Opt, SetRes);
  res.Invert;
end;
procedure TGenCod.ROB_byte_lequ_byte(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_byte_great_byte(Opt, SetRes);
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
procedure TGenCod.ROB_byte_shr_byte(Opt: TxpOperation; SetRes: boolean);  //Desplaza a la derecha
var
  aux: TPicRegister;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  case stoOperation of
  stConst_Const: begin  //compara constantes. Caso especial
    SetROBResultConst_byte(p1^.valInt >> p2^.valInt);
  end;
//  stConst_Variab: begin
//  end;
//  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
//  end;
  stVariab_Const: begin
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
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
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
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
//  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
//  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
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
//  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
//  end;
//  stExpres_Expres:begin
//  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
  end;
end;
procedure TGenCod.ROB_byte_shl_byte(Opt: TxpOperation; SetRes: boolean);   //Desplaza a la izquierda
var
  aux: TPicRegister;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  case stoOperation of
  stConst_Const: begin  //compara constantes. Caso especial
    SetROBResultConst_byte(p1^.valInt << p2^.valInt);
  end;
//  stConst_Variab: begin
//  end;
//  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
//  end;
  stVariab_Const: begin
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
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
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
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
//  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
//  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);   //Se pide Z para el resultado
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
//  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
//  end;
//  stExpres_Expres:begin
//  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
  end;
end;
//////////// Operaciones con Word
procedure TGenCod.ROB_word_asig_word(Opt: TxpOperation; SetRes: boolean);
var
  aux: TPicRegister;
begin
  //Simplifcamos el caso en que p2, sea de tipo p2^
  if not ChangePointerToExpres(p2^) then exit;
  //Realiza la asignación
  if p1^.Sto = stVariab then begin
    case p2^.Sto of
    stConst : begin
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
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
    stVariab: begin
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
      _BANKSEL(p2^.bank);
      _MOVF(p2^.Loffs, toW);
      _BANKSEL(p1^.bank);
      _MOVWF(p1^.Loffs);
      _BANKSEL(p2^.bank);
      _MOVF(p2^.Hoffs, toW);
      _BANKSEL(p1^.bank);
      _MOVWF(p1^.Hoffs);
    end;
    stExpres: begin   //se asume que se tiene en (H,w)
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
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
  end else if p1^.Sto = stVarRefVar then begin
    //Asignación a una variable
    SetResultNull;  //Fomalmente, una aisgnación no devuelve valores en Pascal
    case p2^.Sto of
    stConst : begin
      //Caso especial de asignación a puntero derefrrenciado: variable^
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVF(p1^.offs, toW);
      _MOVWF(FSR.offs);  //direcciona byte bajo
      //Asignación normal
      if p2^.LByte=0 then begin
        //caso especial
        _CLRF(0);
      end else begin
        _MOVLW(p2^.LByte);
        _MOVWF(0);
      end;
      _INCF(FSR.offs, toF);  //direcciona byte alto
      if p2^.HByte=0 then begin
        //caso especial
        _CLRF(0);
      end else begin
        _MOVLW(p2^.HByte);
        _MOVWF(0);
      end;
    end;
    stVariab: begin
      //Caso especial de asignación a puntero dereferenciado: variable^
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVF(p1^.offs, toW);
      _MOVWF(FSR.offs);  //direcciona byte bajo
      //Asignación normal
      _BANKSEL(p2^.bank);  //verifica banco fuente
      _MOVF(p2^.Loffs, toW);
      _MOVWF(0);
      _INCF(FSR.offs, toF);  //direcciona byte alto
      _MOVF(p2^.Hoffs, toW);
      _MOVWF(0);
    end;
    stExpres: begin  //ya está en H,w
      //Caso especial de asignación a puntero dereferenciado: variable^
      aux := GetAuxRegisterByte;
      _BANKSEL(aux.bank);
      _MOVWF(aux.offs);   //Salva W (p2.L)
      //Apunta con p1
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVF(p1^.offs, toW);
      _MOVWF(FSR.offs);  //direcciona a byte bajo
      //Asignación normal
      _BANKSEL(aux.bank);
      _MOVF(aux.offs, toW);   //recupero p2.L
      _MOVWF(0);          //escribe
      _BANKSEL(H.bank);
      _MOVF(H.offs, toW);   //recupero p2.H
      _INCF(FSR.offs, toF);   //apunta a byte alto
      _MOVWF(0);          //escribe
      aux.used := false;
    end;
    else
      GenError('No soportado'); exit;
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure TGenCod.ROB_word_asig_byte(Opt: TxpOperation; SetRes: boolean);
begin
  if p1^.Sto = stVariab then begin
    case p2^.Sto of
    stConst : begin
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
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
    stVariab: begin
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
      _CLRF(p1^.Hoffs);
      _MOVF(p2^.Loffs, toW);
      _MOVWF(p1^.Loffs);
    end;
    stExpres: begin   //se asume que está en w
      SetROBResultExpres_word(Opt);  //Realmente, el resultado no es importante
      _CLRF(p1^.Hoffs);
      _MOVWF(p1^.offs);
    end;
    else
      GenError('No soportado'); exit;
    end;
  end else begin
    GenError('Cannot assign to this Operand.'); exit;
  end;
end;
procedure TGenCod.ROB_word_equal_word(Opt: TxpOperation; SetRes: boolean);
var
  tmp: TPicRegister;
  sale: integer;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  case stoOperation of
  stConst_Const: begin  //compara constantes. Caso especial
    SetROBResultConst_bool(p1^.valInt = p2^.valInt);
  end;
  stConst_Variab: begin
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
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
  stConst_Expres: begin  //la expresión p2 se evaluó p2 esta en W
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
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
  stVariab_Const: begin
    ExchangeP1_P2;  //Convierte a stConst_Variab
    ROB_word_equal_word(Opt, SetRes);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
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
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
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
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    ExchangeP1_P2;  //Convierte a stConst_Expres;
    ROB_word_equal_word(Opt, SetRes);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    ExchangeP1_P2;  //Convierte a stVariab_Expres;
    ROB_word_equal_word(Opt, SetRes);
  end;
  stExpres_Expres:begin
    //La expresión p1, debe estar salvada y p2 en (H,W)
    p1^.SetAsVariab(GetVarWordFromStk);
    //Luego el caso es similar a variable-expresión
    ROB_word_equal_word(Opt, SetRes);
    FreeStkRegisterWord;
  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
  end;
end;
procedure TGenCod.ROB_word_difer_word(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_word_equal_word(Opt, SetRes);
  res.Invert;
end;
procedure TGenCod.ROB_word_great_word(Opt: TxpOperation; SetRes: boolean);
  procedure codVariab_Const;
  {Codifica el caso variable (p1) - constante (p2)}
  var
    sale: integer;
  begin
    if p2^.valInt = $FFFF then begin
      //Nada es mayor que $FFFF
      SetROBResultConst_bool(false);
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
  case stoOperation of
  stConst_Const: begin  //compara constantes. Caso especial
    SetROBResultConst_bool(p1^.valInt > p2^.valInt);
  end;
  stConst_Variab: begin
    if p1^.valInt = 0 then begin
      //0 es mayor que nada
      SetROBResultConst_bool(false);
      GenWarn('Expression will always be FALSE or TRUE.');
      {No se define realmente el mensaje (si es con TRUE o FALSE), porque
      ROB_word_great_word(), es también llamado, por Oper_word_lequ_word para con
      lógica invertida}
    end else begin
      SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
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
  stConst_Expres: begin  //la expresión p2 se evaluó p2 esta en W
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
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
  stVariab_Const: begin
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    codVariab_Const;
  end;
  stVariab_Variab:begin
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    codVariab_Variab;
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en H,W
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
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
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en H,W
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    aux := GetAuxRegisterByte;  //Pide un registro libre
    _MOVWF(aux.offs);  //guarda W
    varTmp := NewTmpVarWord(aux, H);  //Crea variable temporal
    p1^.SetAsVariab(varTmp);  //para que se pueda procesar como variable
    codVariab_Const;      //Lo evalúa como stVariab_Const
    varTmp.Destroy;
    aux.used := false;
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en H,W
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    aux := GetAuxRegisterByte;  //Pide un registro libre
    _MOVWF(aux.offs);  //guarda W
    varTmp := NewTmpVarWord(aux, H);  //Crea variable temporal
    p1^.SetAsVariab(varTmp);  //para que se pueda procesar como variable
    codVariab_Variab;      //Lo evalúa como stVariab_Variab;
    varTmp.Destroy;
    aux.used := false;
  end;
  stExpres_Expres:begin
    //La expresión p1, debe estar salvada y p2 en (H,W)
    p1^.SetAsVariab(GetVarWordFromStk);
    //Luego el caso es similar a variable-expresión
    ROB_word_great_word(Opt, SetRes);
    FreeStkRegisterWord;
  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
  end;
end;
procedure TGenCod.ROB_word_add_word(Opt: TxpOperation; SetRes: boolean);
var
  aux: TPicRegister;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  case stoOperation of
  stConst_Const: begin
    if p1^.valInt+p2^.valInt <256 then begin
      //Optimiza
      SetROBResultConst_byte(p1^.valInt+p2^.valInt);
    end else begin
      SetROBResultConst_word(p1^.valInt+p2^.valInt);
    end;
  end;
  stConst_Variab: begin
    SetROBResultExpres_word(Opt);
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
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en (H,W)
    SetROBResultExpres_word(Opt);
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
  stVariab_Const: begin
    SetROBResultExpres_word(Opt);
    _MOVLW(p2^.HByte);      //Carga más peso del dato 1
    _ADDWF(p1^.Hoffs,toW);  //Suma más peso del dato 2
    _MOVWF(H.offs);         //Guarda el resultado
    _MOVLW(p2^.LByte);      //Carga menos peso del dato 1
    _ADDWF(p1^.Loffs,toW);  //Suma menos peso del dato 2, deja en W
    _BTFSC(STATUS,_C);     //Hubo acarreo anterior?
    _INCF(H.offs, toF);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_word(Opt);
    _MOVF(p1^.Hoffs, toW);  //Carga mayor peso del dato 1
    _ADDWF(p2^.Hoffs,toW);  //Suma mayor peso del dato 2
    _MOVWF(H.offs);         //Guarda mayor peso del resultado
    _MOVF(p1^.Loffs, toW);  //Carga menos peso del dato 1
    _ADDWF(p2^.Loffs,toW);  //Suma menos peso del dato 2, deja en W
    _BTFSC(STATUS,_C);     //Hubo acarreo anterior?
    _INCF(H.offs, toF);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en (H,W)
    SetROBResultExpres_word(Opt);
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
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,W)
    SetROBResultExpres_word(Opt);
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
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,W)
    SetROBResultExpres_word(Opt);
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
  stExpres_Expres:begin
    SetROBResultExpres_word(Opt);
    //p1 está salvado en pila y p2 en (_H,W)
    p1^.SetAsVariab(GetVarWordFromStk);  //Convierte a variable
    //Luego el caso es similar a stVariab_Expres
    ROB_word_add_word(Opt, SetRes);
    FreeStkRegisterWord;   //libera pila, obtiene dirección
  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
  end;
end;
procedure TGenCod.ROB_word_add_byte(Opt: TxpOperation; SetRes: boolean);
var
  aux: TPicRegister;
begin
  case stoOperation of
  stConst_Const: begin
    if p1^.valInt+p2^.valInt <256 then begin
      //Optimiza
      SetROBResultConst_byte(p1^.valInt+p2^.valInt);
    end else begin
      SetROBResultConst_word(p1^.valInt+p2^.valInt);
    end;
  end;
  stConst_Variab: begin
    SetROBResultExpres_word(Opt);
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
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en (W)
    SetROBResultExpres_word(Opt);
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
  stVariab_Const: begin
    SetROBResultExpres_word(Opt);
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
  stVariab_Variab:begin
    SetROBResultExpres_word(Opt);
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
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en (_H,W)
    SetROBResultExpres_word(Opt);
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
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,W)
    SetROBResultExpres_word(Opt);
    _addlw(p2^.LByte);     //Suma menos peso del dato 2, deja en W
    _BANKSEL(H.bank);      //se cambia primero el banco, por si acaso.
    _btfsc(STATUS,_C);    //Hubo acarreo anterior?
    _incf(H.offs, toF);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,W)
    SetROBResultExpres_word(Opt);
    _BANKSEL(p2^.bank);
    _addwf(p2^.Loffs,toW);         //Suma menos peso del dato 2, deja en W
    _BANKSEL(H.bank);      //se cambia primero el banco, por si acaso.
    _btfsc(STATUS,_C);    //Hubo acarreo anterior?
    _incf(H.offs, toF);
  end;
  stExpres_Expres:begin
    SetROBResultExpres_word(Opt);
    //p1 está salvado en pila y p2 en (_H,W)
    p1^.SetAsVariab(GetVarWordFromStk);  //Convierte a variable
    //Luego el caso es similar a stVariab_Expres
    ROB_word_add_byte(Opt, SetRes);
    FreeStkRegisterWord;   //libera pila
  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
  end;
end;
procedure TGenCod.ROB_word_sub_word(Opt: TxpOperation; SetRes: boolean);
var
  aux: TPicRegister;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;
  case stoOperation of
  stConst_Const: begin
    if p1^.valInt-p2^.valInt < 0 then begin
      genError('Numeric value exceeds a word range.');
      exit;
    end;
    if p1^.valInt-p2^.valInt <256 then begin
      //Optimiza
      SetROBResultConst_byte(p1^.valInt-p2^.valInt);
    end else begin
      SetROBResultConst_word(p1^.valInt-p2^.valInt);
    end;
  end;
  stConst_Variab: begin
    SetROBResultExpres_word(Opt);
    _movf (p2^.Hoffs,toW);  //p2->w
    _SUBLW(p1^.HByte);     //p1 - W -W
    _movwf(H.offs);
    _movf (p2^.Loffs,toW);  //p2-W
    _SUBLW(p1^.LByte);      //p1-W->w
    _btfss(STATUS, _C);
    _decf(H.offs,toF);
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en (H,W)
    SetROBResultExpres_word(Opt);
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
  stVariab_Const: begin
    SetROBResultExpres_word(Opt);
    _movlw(p2^.HByte);
    _subwf(p1^.Hoffs,toW);
    _movwf(H.offs);
    _movlw(p2^.LByte);
    _subwf(p1^.Loffs,toW);
    _btfss(STATUS, _C);
    _decf(H.offs,toF);
  end;
  stVariab_Variab:begin  //p1 - p2
    SetROBResultExpres_word(Opt);
    _movf (p2^.Hoffs,toW);
    _subwf(p1^.Hoffs,toW);
    _movwf(H.offs);
    _movf (p2^.Loffs,toW);
    _subwf(p1^.Loffs,toW);
    _btfss(STATUS, _C);
    _decf(H.offs,toF);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en (H,W)
    SetROBResultExpres_word(Opt);
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
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,W)
    SetROBResultExpres_word(Opt);
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
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,W)
    SetROBResultExpres_word(Opt);
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
  stExpres_Expres:begin
    SetROBResultExpres_word(Opt);
    //p1 está salvado en pila y p2 en (_H,W)
    p1^.SetAsVariab(GetVarWordFromStk);  //Convierte a variable
    //Luego el caso es similar a stVariab_Expres
    ROB_word_sub_word(Opt, SetRes);
    FreeStkRegisterWord;   //libera pila, obtiene dirección
  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
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
procedure TGenCod.ROB_word_umulword_word(Opt: TxpOperation; SetRes: boolean);
begin
  case stoOperation of
  stConst_Const:begin  //producto de dos constantes. Caso especial
    SetROBResultConst_word((p1^.valInt*p2^.valInt) and $FFFF);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
//  stConst_Variab: begin
//    SetROBResultExpres_word(Opt);
//    _BANKSEL(p2^.bank);
//    _MOVF(p2^.offs, toW);
//    _BANKSEL(H.bank);
//    _MOVWF(H.offs);
//    _MOVLW(p1^.valInt);
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
//    _BANKSEL(H.bank);
//    _MOVWF(H.offs);
//    _MOVLW(p1^.valInt);
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  stVariab_Const: begin
//    SetROBResultExpres_byte(Opt);
//    _BANKSEL(p1^.bank);
//    _MOVF(p1^.offs, toW);
//    _BANKSEL(H.bank);
//    _MOVWF(H.offs);
//    _MOVLW(p2^.valInt);
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  stVariab_Variab:begin
//    SetROBResultExpres_byte(Opt);
//    _BANKSEL(p1^.bank);
//    _MOVF(p1^.offs, toW);
//    _BANKSEL(H.bank);
//    _MOVWF(H.offs);
//    _BANKSEL(p2^.bank);
//    _MOVF(p2^.offs, toW);
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
//    _BANKSEL(H.bank);
//    _MOVWF(H.offs);  //p2 -> H
//    _BANKSEL(p1^.bank);
//    _MOVF(p1^.offs, toW); //p1 -> W
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
//    _MOVWF(H.offs);  //p1 -> H
//    _MOVLW(p2^.valInt); //p2 -> W
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
//    _BANKSEL(H.bank);
//    _MOVWF(H.offs);  //p1 -> H
//    _BANKSEL(p2^.bank);
//    _MOVF(p2^.offs, toW); //p2 -> W
//    _CALL(f_byteXbyte_byte.adrr);
//    if FirstPass then f_byteXbyte_byte.AddCaller;
//  end;
//  stExpres_Expres:begin
//    SetROBResultExpres_byte(Opt);
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
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
  end;
end;
procedure TGenCod.ROB_word_and_byte(Opt: TxpOperation; SetRes: boolean);
begin
  case stoOperation of
  stConst_Const: begin
    //Optimiza
    SetROBResultConst_byte(p1^.valInt and p2^.valInt);
  end;
  stConst_Variab: begin
    SetROBResultExpres_byte(Opt);
    _movlw(p1^.LByte);      //Carga menos peso del dato 1
    _BANKSEL(p2^.bank);
    _andwf(p2^.Loffs,toW);  //deja en W
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en (W)
    SetROBResultExpres_byte(Opt);
    _andlw(p1^.LByte);      //Deja en W
  end;
  stVariab_Const: begin
    SetROBResultExpres_byte(Opt);
    _BANKSEL(p1^.bank);
    _MOVF(p1^.Loffs, toW);
    _ANDLW(p2^.LByte);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);
    _BANKSEL(p1^.bank);
    _MOVF(p1^.Loffs, toW);
    _BANKSEL(p2^.bank);
    _ANDWF(p2^.Loffs, toW);
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en (_H,W)
    SetROBResultExpres_byte(Opt);
    _BANKSEL(p1^.bank);
    _ANDWF(p1^.Loffs, toW);
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,W)
    SetROBResultExpres_byte(Opt);
    _ANDLW(p2^.LByte);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,W)
    SetROBResultExpres_byte(Opt);
    _BANKSEL(p2^.bank);
    _ANDWF(p2^.Loffs, toW);
  end;
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
    //p1 está salvado en pila y p2 en (W)
    p1^.SetAsVariab(GetVarWordFromStk);  //Convierte a variable
    //Luego el caso es similar a stVariab_Expres
    _BANKSEL(p1^.bank);
    _ANDWF(p1^.Loffs, toW);
    FreeStkRegisterWord;   //libera pila
  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
  end;
end;
procedure TGenCod.ROU_addr_word(Opr: TxpOperator; SetRes: boolean);
{Devuelve la dirección de una variable.}
begin
  case p1^.Sto of
  stConst : begin
    genError('Cannot obtain address of constant.');
  end;
  stVariab: begin
    //Es una variable normal
    //La dirección de una variable es constante
    SetResultConst(typByte);
    //No se usa p1^.offs, porque solo retorna 7 bits;
    res.valInt := p1^.rVar.addr and $ff;
  end;
  stExpres: begin  //ya está en STATUS.Z
    genError('Cannot obtain address of an expression.');
  end;
  else
    genError('Cannot obtain address of this operand.');
  end;
end;

//////////// Operaciones con Dword
procedure TGenCod.ROB_dword_asig_byte(Opt: TxpOperation; SetRes: boolean);
begin
  if p1^.Sto <> stVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.Sto of
  stConst : begin
    SetROBResultExpres_dword(Opt);  //Realmente, el resultado no es importante
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
  stVariab: begin
    SetROBResultExpres_dword(Opt);  //Realmente, el resultado no es importante
    _CLRF(p1^.Uoffs);
    _CLRF(p1^.Eoffs);
    _CLRF(p1^.Hoffs);
    _MOVF(p2^.Loffs, toW);
    _MOVWF(p1^.Loffs);
  end;
  stExpres: begin   //se asume que está en w
    SetROBResultExpres_dword(Opt);  //Realmente, el resultado no es importante
    _CLRF(p1^.Uoffs);
    _CLRF(p1^.Eoffs);
    _CLRF(p1^.Hoffs);
    _MOVWF(p1^.offs);
  end;
  else
    GenError('No soportado'); exit;
  end;
end;
procedure TGenCod.ROB_dword_asig_word(Opt: TxpOperation; SetRes: boolean);
begin
  if p1^.Sto <> stVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.Sto of
  stConst : begin
    SetROBResultExpres_dword(Opt);  //Realmente, el resultado no es importante
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
  stVariab: begin
    SetROBResultExpres_dword(Opt);  //Realmente, el resultado no es importante
    _CLRF(p1^.Uoffs);
    _CLRF(p1^.Eoffs);
    _MOVF(p2^.Hoffs, toW);
    _MOVWF(p1^.Hoffs);
    _MOVF(p2^.Loffs, toW);
    _MOVWF(p1^.Loffs);
  end;
  stExpres: begin   //se asume que está en w
    SetROBResultExpres_dword(Opt);  //Realmente, el resultado no es importante
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
procedure TGenCod.ROB_dword_asig_dword(Opt: TxpOperation; SetRes: boolean);
begin
  if p1^.Sto <> stVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.Sto of
  stConst : begin
    SetROBResultExpres_dword(Opt);  //Realmente, el resultado no es importante
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
  stVariab: begin
    SetROBResultExpres_dword(Opt);  //Realmente, el resultado no es importante
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
  stExpres: begin   //se asume que está en w
    SetROBResultExpres_dword(Opt);  //Realmente, el resultado no es importante
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
procedure TGenCod.ROB_dword_equal_dword(Opt: TxpOperation; SetRes: boolean);
var
  sale1, sale2, sale3: integer;
begin
  case stoOperation of
  stConst_Const: begin  //compara constantes. Caso especial
    SetROBResultConst_bool(p1^.valInt = p2^.valInt);
  end;
  stConst_Variab: begin
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
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
  stConst_Expres: begin  //la expresión p2 se evaluó y está en UEHW
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
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
  stVariab_Const: begin
    ExchangeP1_P2;  //Convierte a stConst_Variab
    ROB_dword_equal_dword(Opt, SetRes);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
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
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
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
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    ExchangeP1_P2;  //Convierte a stConst_Expres;
    ROB_dword_equal_dword(Opt, SetRes);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    ExchangeP1_P2;  //Convierte a stVariab_Expres;
    ROB_dword_equal_dword(Opt, SetRes);
  end;
  stExpres_Expres:begin
    //La expresión p1, debe estar salvada y p2 en (H,W,E,U)
    p1^.SetAsVariab(GetVarDWordFromStk);
    //Luego el caso es similar a variable-expresión
    ROB_dword_equal_dword(Opt, SetRes);
    FreeStkRegisterdWord;
  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
  end;
end;
procedure TGenCod.ROB_dword_difer_dword(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_dword_equal_dword(Opt, SetRes);
  res.Invert;
end;
procedure TGenCod.ROB_dword_add_dword(Opt: TxpOperation; SetRes: boolean);
var
  aux: TPicRegister;
  varTmp: TxpEleVar;
begin
  case stoOperation of
  stConst_Const: begin
    if p1^.valInt+p2^.valInt < $FF then begin
      //Optimiza
      SetROBResultConst_byte(p1^.valInt+p2^.valInt);
    end else if p1^.valInt+p2^.valInt < $FFFF then begin
      //Optimiza
      SetROBResultConst_word(p1^.valInt+p2^.valInt);
    end else begin
      SetROBResultConst_dword(p1^.valInt+p2^.valInt);
    end;
  end;
  stConst_Variab: begin
    SetROBResultExpres_dword(Opt);
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
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en (H,W)
    if SetRes then SetROBResultExpres_dword(Opt); //Se fija aquí el resultado
    //K + WHEU -> WHEU, se puede manejar como asignación con sums
    aux := GetAuxRegisterByte;  //Pide un registro libre
    _MOVWF(aux.offs);  //guarda W
    varTmp := NewTmpVarDword(aux, H, E, U);  //Crea variable temporal, con los RT
    p2^.SetAsVariab(varTmp);  //Convierte p2 a variable
    ExchangeP1_P2;  //Convierte a p1 := p1 + K;
    ROB_dword_aadd_dword(Opt, false);  //compila como autosuma
    _MOVF(aux.offs, toW);  //devuelve byet bajo en W
    aux.used := false;
    varTmp.Destroy;  //Destruye la variable
  end;
  stVariab_Const: begin
    ExchangeP1_P2;  //Convierte a stConst_Variab
    ROB_dword_add_dword(Opt, SetRes);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_dword(Opt);
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
//  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en (H,W)
//    SetROBResultExpres_word(Opt);
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
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,W)
    if SetRes then SetROBResultExpres_dword(Opt); //Se fija aquí el resultado
    //WHEU + K -> WHEU, se puede manejar como asignación con suma
    aux := GetAuxRegisterByte;  //Pide un registro libre
    _MOVWF(aux.offs);  //gaurda W
    varTmp := NewTmpVarDword(aux, H, E, U);  //Crea variable temporal
    p1^.SetAsVariab(varTmp);  //Convierte p1 a variable
    ROB_dword_aadd_dword(Opt, false);  //compila como autosuma
    _MOVF(aux.offs, toW);  //devuelve byet bajo en W
    aux.used := false;
    varTmp.Destroy;  //Destruye la variable
  end;
//  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,W)
//    SetROBResultExpres_word(Opt);
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
//  stExpres_Expres:begin
//    SetROBResultExpres_word(Opt);
//    //p1 está salvado en pila y p2 en (_H,W)
//    p1^.Sto := stVariab;  //Convierte a variable
//    p1^.rVar := GetVarWordFromStk;
//    stoOperation := TStoOperandsROB((Ord(p1^.Sto) << 2) or ord(p2^.Sto));
//    //Luego el caso es similar a stVariab_Expres
//    ROB_word_add_word;
//    FreeStkRegisterByte(spH);   //libera pila, obtiene dirección
//    FreeStkRegisterByte(spL);   //libera pila, obtiene dirección
//  end;
  else
    genError('Cannot Compile: "%s"', [Opt.OperationString]);
  end;
end;
procedure TGenCod.ROB_dword_aadd_dword(Opt: TxpOperation; SetRes: boolean);
begin
  if p1^.Sto <> stVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.Sto of
  stConst : begin
    if SetRes then SetROBResultExpres_dword(Opt);  //Realmente, el resultado no es importante
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
  stVariab: begin
    if SetRes then SetROBResultExpres_dword(Opt);  //Realmente, el resultado no es importante
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
  stExpres: begin   //Se asume que está en U,E,H,w
    if SetRes then SetROBResultExpres_dword(Opt);  //Realmente, el resultado no es importante
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
procedure TGenCod.ROB_char_asig_char(Opt: TxpOperation; SetRes: boolean);
begin
  if p1^.Sto <> stVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.Sto of
  stConst : begin
    SetROBResultExpres_char(Opt);  //Realmente, el resultado no es importante
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
  stVariab: begin
    SetROBResultExpres_char(Opt);  //Realmente, el resultado no es importante
    _BANKSEL(p2^.bank);  //verifica banco destino
    _MOVF(p2^.offs, toW);
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVWF(p1^.offs);
  end;
  stExpres: begin  //ya está en w
    SetROBResultExpres_char(Opt);  //Realmente, el resultado no es importante
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVWF(p1^.offs);
  end;
  else
    GenError('No soportado'); exit;
  end;
end;
procedure TGenCod.ROB_char_equal_char(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_byte_equal_byte(Opt, SetRes);  //es lo mismo
end;
procedure TGenCod.ROB_char_difer_char(Opt: TxpOperation; SetRes: boolean);
begin
  ROB_byte_difer_byte(Opt, SetRes); //es lo mismo
end;
//////////// Operaciones con punteros
procedure TGenCod.ROB_pointer_add_byte(Opt: TxpOperation; SetRes: boolean);
{Implementa la suma de un puntero (a cualquier tipo) y un byte.}
var
  ptrType: TxpEleType;
begin
  {Guarda la referencia al tipo puntero, porque:
  * Se supone que este tipo lo define el usuario y no se tiene predefinido.
  * Se podrían definir varios tipos de puntero) así que no se tiene una
  referencia estática
  * Conviene manejar esto de forma dinámica para dar flexibilidad al lenguaje}
  ptrType := p1^.Typ;   //Se ahce aquí porque después puede cambiar p1^.
  //La suma de un puntero y un byte, se procesa, como una suma de bytes
  ROB_byte_add_byte(Opt, SetRes);
  //Devuelve byte, oero debe devolver el tipo puntero
  case res.Sto of
  stConst: res.SetAsConst(ptrType);  //Cambia el tipo a la constante
  //stVariab: res.SetAsVariab(res.rVar);
  {Si devuelve variable, solo hay dos posibilidades:
   1. Que sea la variable puntero, por lo que no hay nada que hacer, porque ya tiene
      el tipo puntero.
   2. Que sea la variable byte (y que la otra era constante puntero 0 = nil). En este
      caso devolverá el tipo Byte, lo cual tiene cierto sentido.}
  stExpres: res.SetAsExpres(ptrType);  //Cambia tipo a la expresión
  end;
end;
procedure TGenCod.ROB_pointer_sub_byte(Opt: TxpOperation; SetRes: boolean);
{Implementa la resta de un puntero (a cualquier tipo) y un byte.}
var
  ptrType: TxpEleType;
begin
  //La explicación es la misma que para la rutina ROB_pointer_add_byte
  ptrType := p1^.Typ;
  ROB_byte_sub_byte(Opt, SetRes);
  case res.Sto of
  stConst: res.SetAsConst(ptrType);
  stExpres: res.SetAsExpres(ptrType);
  end;
end;
procedure TGenCod.ROU_derefPointer(Opr: TxpOperator; SetRes: boolean);
{Implementa el operador de desreferencia "^", para Opr que se supone debe ser
 categoria "tctPointer", es decir, puntero a algún tipo de dato.}
var
  tmpVar: TxpEleVar;
begin
  case p1^.Sto of
  stConst : begin
    //Caso especial. Cuando se tenga algo como: TPunteroAByte($FF)^
    //Se asume que devuelve una variable de tipo Byte.
    tmpVar := CreateTmpVar('', typByte);
    tmpVar.addr0 := p1^.valInt;  //Fija dirección de constante
    SetROUResultVariab(tmpVar);
  end;
  stVariab: begin
    //Caso común: ptrWord^
    //La desreferencia de una variable "tctPointer" es un stVarRefVar.
    SetROUResultVarRef(p1^.rVar);
  end;
  stExpres: begin
    //La expresión Esta en RT, pero es una dirección, no un valor
    SetROUResultExpRef(nil, p1^.Typ);
  end;
  else
    genError('Not implemented: "%s"', [Opr.OperationString]);
  end;
end;
///////////// Funciones del sistema
procedure TGenCod.codif_1mseg;
//Codifica rutina de retardo de 1mseg.
begin
  PutFwdComm(';1 msec routine.');
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
  LoadToRT(res);   //Carga en registro de trabajo
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
{Se debe dejar en los registros de trabajo, el valor del parámetro indicado.}
var
  curFunTyp: TxpEleType;
  parentNod: TxpEleCodeCont;
  curFun: TxpEleFun;
  posExit: TSrcPos;
//  adrReturn: word;
begin
  //TreeElems.curNode, debe ser de tipo "Body".
  parentNod := TreeElems.CurCodeContainer;  //Se supone que nunca debería fallar
  posExit := cIn.ReadSrcPos;  //Guarda para el AddExitCall()
  if parentNod.idClass = eltMain then begin
    //Es el cuerpo del programa principal
    _SLEEP;   //Así se termina un programa en PicPas
  end else if parentNod.idClass = eltFunc then begin
    //Es el caso común, un exit() en procedimientos.
    //"parentNod" debe ser de tipo TxpEleFun
    curFun := TxpEleFun(parentNod);
    if curFun.IsInterrupt then begin
      GenError('Cannot use exit() in an INTERRUPT.');
      exit;
    end;
    //Codifica el retorno
    curFunTyp := curFun.typ;
    if curFunTyp = typNull then begin
      //No retorna valores. Es solo procedimiento
      _RETURN;
      //No hay nada, más que hacer
    end else begin
      //Se espera el valor devuelto
      if not CaptureTok('(') then exit;
      GetExpressionE(0, pexPARSY);  //captura parámetro
      if HayError then exit;   //aborta
      //Verifica fin de parámetros
      if not CaptureTok(')') then exit;
      //El resultado de la expresión está en "res".
      if curFunTyp <> res.Typ then begin
        GenError('Expected a "%s" expression.', [curFunTyp.name]);
        exit;
      end;
      LoadToRT(res, true);  //Carga expresión en RT y genera RETURN o RETLW
    end;
  end else begin
    //Faltaría implementar en cuerpo de TxpEleUni
    GenError('Syntax error.');
  end;
  //Lleva el registro de las llamadas a exit()
  if FirstPass then begin
    //CurrBank debe ser el banco con el que se llamó al RETURN.
    parentNod.AddExitCall(posExit, parentNod.CurrBlockID, CurrBank);
  end;
  res.SetAsNull;  //No es función
end;
procedure TGenCod.fun_Inc(fun: TxpEleFun);
begin
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    GenError('Cannot increase a constant.'); exit;
  end;
  stVariab: begin
    if (res.Typ = typByte) or (res.Typ = typChar) then begin
      _BANKSEL(res.bank);
      _INCF(res.offs, toF);
    end else if res.Typ = typWord then begin
      _BANKSEL(res.bank);
      _INCF(res.Loffs, toF);
      _BTFSC(STATUS, _Z);
      _INCF(res.Hoffs, toF);
    end else if res.Typ = typDWord then begin
      _BANKSEL(res.bank);
      _INCF(res.Loffs, toF);
      _BTFSC(STATUS, _Z);
      _INCF(res.Hoffs, toF);
      _BTFSC(STATUS, _Z);
      _INCF(res.Eoffs, toF);
      _BTFSC(STATUS, _Z);
      _INCF(res.Uoffs, toF);
    end else if res.Typ.catType = tctPointer then begin
      //Es puntero corto
      _BANKSEL(res.bank);
      _INCF(res.offs, toF);
    end else begin
      GenError('Invalid parameter type: %s', [res.Typ.name]);
      exit;
    end;
  end;
//  stVarRefVar: begin
//    if (res.Typ = typByte) or (res.Typ = typChar) then begin
//      _BANKSEL(res.bank);
//      _INCF(res.offs, toF);
//    end else if res.Typ = typWord then begin
//      _BANKSEL(res.bank);
//      _INCF(res.Loffs, toF);
//      _BTFSC(STATUS, _Z);
//      _INCF(res.Hoffs, toF);
//    end else if res.Typ = typDWord then begin
//      _BANKSEL(res.bank);
//      _INCF(res.Loffs, toF);
//      _BTFSC(STATUS, _Z);
//      _INCF(res.Hoffs, toF);
//      _BTFSC(STATUS, _Z);
//      _INCF(res.Eoffs, toF);
//      _BTFSC(STATUS, _Z);
//      _INCF(res.Uoffs, toF);
//    end else if res.Typ.catType = tctPointer then begin
//      //Es puntero corto
//      _BANKSEL(res.bank);
//      _INCF(res.offs, toF);
//    end else begin
//      GenError('Invalid parameter type: %s', [res.Typ.name]);
//      exit;
//    end;
//  end;
  stExpres: begin  //se asume que ya está en (_H,w)
    GenError('Cannot increase an expression.'); exit;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
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
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    GenError('Cannot decrease a constant.'); exit;
  end;
  stVariab: begin
    if (res.Typ = typByte) or (res.Typ = typChar) then begin
      _BANKSEL(res.bank);
      _DECF(res.offs, toF);
    end else if res.Typ = typWord then begin
      _BANKSEL(res.bank);
      _MOVF(res.Loffs, toW);
      _BTFSC(STATUS, _Z);
      _DECF(res.Hoffs, toF);
      _DECF(res.Loffs, toF);
    end else if res.Typ = typDWord then begin
      _BANKSEL(res.bank);
      _MOVLW(1);
      _subwf(res.Loffs, toF);
      _BTFSS(STATUS, _C);
      _subwf(RES.Hoffs, toF);
      _BTFSS(STATUS, _C);
      _subwf(RES.Eoffs, toF);
      _BTFSS(STATUS, _C);
      _subwf(RES.Uoffs, toF);
    end else if res.Typ.catType = tctPointer then begin
      //Es puntero corto
      _BANKSEL(res.bank);
      _DECF(res.offs, toF);
    end else begin
      GenError('Invalid parameter type: %s', [res.Typ.name]);
      exit;
    end;
  end;
  stExpres: begin  //se asume que ya está en (_H,w)
    GenError('Cannot decrease an expression.'); exit;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
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
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    if res.Typ = typChar then begin
      SetResultConst(typByte);  //Solo cambia el tipo, no el valor
      //No se usa SetROBResultConst_byte, porque no estamos en ROP
    end else begin
      GenError('Cannot convert to ordinal.'); exit;
    end;
  end;
  stVariab: begin
    if res.Typ = typChar then begin
      //Sigue siendo variable
      tmpVar := CreateTmpVar('', typByte);   //crea variable temporal Byte
      tmpVar.addr0 := res.rVar.addr0; //apunta al mismo byte
      SetResultVariab(tmpVar);  //Actualiza "res"
    end else begin
      GenError('Cannot convert to ordinal.'); exit;
    end;
  end;
  stExpres: begin  //se asume que ya está en (w)
    if res.Typ = typChar then begin
      //Es la misma expresión, solo que ahora es Byte.
      res.SetAsExpres(typByte); //No se puede usar SetROBResultExpres_byte, porque no hay p1 y p2
    end else begin
      GenError('Cannot convert to ordinal.'); exit;
    end;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
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
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    if res.Typ = typByte then begin
      SetResultConst(typChar);  //Solo cambia el tipo, no el valor
      //No se usa SetROBResultConst_char, porque no estamos en ROP
    end else begin
      GenError('Cannot convert to char.'); exit;
    end;
  end;
  stVariab: begin
    if res.Typ = typByte then begin
      //Sigue siendo variable
      tmpVar := CreateTmpVar('', typChar);   //crea variable temporal
      tmpVar.addr0 := res.rVar.addr0; //apunta al mismo byte
      SetResultVariab(tmpVar);
    end else begin
      GenError('Cannot convert to char.'); exit;
    end;
  end;
  stExpres: begin  //se asume que ya está en (w)
    if res.Typ = typByte then begin
      //Es la misma expresión, solo que ahora es Char.
      res.SetAsExpres(typChar); //No se puede usar SetROBResultExpres_char, porque no hay p1 y p2;
    end else begin
      GenError('Cannot convert to char.'); exit;
    end;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Bit(fun: TxpEleFun);
{Convierte byte, o boolean a bit}
var
  tmpVar: TxpEleVar;
begin
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    if res.Typ = typByte then begin
      if res.valInt= 0 then begin
        SetResultConst(typBit);  //No se usa SetROBResultConst_bit, porque no estamos en ROP
        res.valBool := false;
      end else begin
        SetResultConst(typBit);  //No se usa SetROBResultConst_bit, porque no estamos en ROP
        res.valBool := true;
      end;
    end else begin
      GenError('Cannot convert to bit.'); exit;
    end;
  end;
  stVariab: begin
    if res.Typ = typByte then begin
      //No se usa SetROUResultExpres_bit(true), porque no se tiene el operando en p1^
      SetResultExpres(typBit);
      res.Inverted := true;
      //Se asumirá que cualquier valor diferente de cero, devuelve 1
      _MOVF(res.Loffs, toW);    //el resultado aparecerá en Z, invertido
      {Notar que se ha usado res.Loff, para apuntar a la dirección de la variable byte.
      Si se hubeise usado solo res.off, apuntaría a una dirección del tipo bit}
    end else if res.Typ = typBool then begin
      //Sigue siendo variable
      tmpVar := CreateTmpVar('', typBit);   //crea variable temporal
      tmpVar.addr0 := res.rVar.addr0; //Apunta al mismo byte
      tmpVar.bit0  := res.rVar.bit0;  //Apunta al mismo bit
      SetResultVariab(tmpVar, res.Inverted);   //mantiene lógica
    end else begin
      GenError('Cannot convert to bit.'); exit;
    end;
  end;
  stExpres: begin  //se asume que ya está en (w)
    if res.Typ = typByte then begin
      SetResultExpres(typBit); //No se usa SetROUResultExpres_bit, porque no se tiene el operando en p1^
      res.Inverted := true;
      _ADDLW(0);   //el resultado aparecerá en Z, invertido
    end else begin
      GenError('Cannot convert to bit.'); exit;
    end;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Bool(fun: TxpEleFun);
{Convierte byte, o bit a boolean}
var
  tmpVar: TxpEleVar;
begin
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    if res.Typ = typByte then begin
      if res.valInt= 0 then begin
        SetResultConst(typBool);  //No se usa SetROBResultConst_bit, porque no estamos en ROP
        res.valBool := false;
      end else begin
        SetResultConst(typBool);  //No se usa SetROBResultConst_bit, porque no estamos en ROP
        res.valBool := true;
      end;
    end else begin
      GenError('Cannot convert to boolean.'); exit;
    end;
  end;
  stVariab: begin
    if res.Typ = typByte then begin
      //No se usa SetROUResultExpres_bool(true), porque no se tiene el operando en p1^
      SetResultExpres(typBool);
      res.Inverted := true;
      //Se asumirá que cualquier valor diferente de cero, devuelve 1
      _MOVF(res.Loffs, toW);    //el resultado aparecerá en Z, invertido
      {Notar que se ha usado res.Loff, para apuntar a la dirección de la variable byte.
      Si se hubeise usado solo res.off, apuntaría a una dirección del tipo bit}
    end else if res.Typ = typBit then begin
      //Sigue siendo variable
      tmpVar := CreateTmpVar('', typBool);   //crea variable temporal
      tmpVar.addr0 := res.rVar.addr0; //Apunta al mismo byte
      tmpVar.bit0  := res.rVar.bit0;  //Apunta al mismo bit
      SetResultVariab(tmpVar, res.Inverted);   //mantiene lógica
    end else begin
      GenError('Cannot convert to boolean.'); exit;
    end;
  end;
  stExpres: begin  //se asume que ya está en (w)
    if res.Typ = typByte then begin
      SetResultExpres(typBool); //No se usa SetROUResultExpres_bit, porque no se tiene el operando en p1^
      res.Inverted := true;
      _ADDLW(0);   //el resultado aparecerá en Z, invertido
    end else begin
      GenError('Cannot convert to boolean.'); exit;
    end;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
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
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
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
  stVariab: begin
    if res.Typ = typBit then begin
      SetResultExpres(typByte);
      _CLRW;
      _BTFSC(res.Boffs, res.bit);
      _MOVLW(1);  //devuelve 1
      //Es lo mismo
    end else if res.Typ = typChar then begin
      //Crea varaible que apunte al byte bajo
      tmpVar := CreateTmpVar('', typByte);   //crea variable temporal Byte
      tmpVar.addr0 := res.rVar.addr0;  //apunta al mismo byte
      SetResultVariab(tmpVar);
    end else if res.Typ = typByte then begin
      //Es lo mismo
    end else if res.Typ = typWord then begin
      //Crea varaible que apunte al byte bajo
      tmpVar := CreateTmpVar('', typByte);   //crea variable temporal Byte
      tmpVar.addr0 := res.rVar.addr0;  //apunta al mismo byte
      SetResultVariab(tmpVar);
    end else if res.Typ = typDWord then begin
      //CRea varaible que apunte al byte bajo
      tmpVar := CreateTmpVar('', typByte);   //crea variable temporal Byte
      tmpVar.addr0 := res.rVar.addr0;  //apunta al mismo byte
      SetResultVariab(tmpVar);
    end else begin
      GenError('Cannot convert to byte.'); exit;
    end;
  end;
  stExpres: begin  //se asume que ya está en (w)
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
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
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
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
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
  stVariab: begin
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
      tmpVar.addr0 := res.rVar.addr0; //apunta al byte L
      tmpVar.addr1 := res.rVar.addr1; //apunta al byte H
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
  stExpres: begin  //se asume que ya está en (w)
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
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_DWord(fun: TxpEleFun);
begin
  if not CaptureTok('(') then exit;
  res := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit;   //aborta
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
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
  stVariab: begin
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
  stExpres: begin  //se asume que ya está en (w)
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
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_SetAsInput(fun: TxpEleFun);
begin
  if not CaptureTok('(') then exit;
  GetExpressionE(0, pexPARSY);  //captura parámetro
  if HayError then exit;   //aborta
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    GenError('PORT or BIT variable expected.'); exit;
  end;
  stVariab: begin
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
  stExpres: begin  //se asume que ya está en (w)
    GenError('PORT variable expected.'); exit;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_SetAsOutput(fun: TxpEleFun);
begin
  if not CaptureTok('(') then exit;
  GetExpressionE(0, pexPARSY);  //captura parámetro
  if HayError then exit;   //aborta
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    GenError('PORT variable expected.'); exit;
  end;
  stVariab: begin
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
  stExpres: begin  //se asume que ya está en (w)
    GenError('PORT variable expected.'); exit;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_SetBank(fun: TxpEleFun);
{Define el banco actual}
begin
  if not CaptureTok('(') then exit;
  GetExpressionE(0, pexPARSY);  //captura parámetro
  if HayError then exit;   //aborta
  case res.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    if (res.Typ = typByte) or (res.Typ = typWord) or (res.Typ = typDWord) then begin
      //ya es Word
      CurrBank := 255;   //para forzar el cambio
      _BANKSEL(res.valInt);
    end else begin
      GenError('Number expected.'); exit;
    end;
  end;
  stVariab, stExpres: begin  //se asume que ya está en (w)
    GenError('A constant expected.'); exit;
  end;
  else
    genError('Not implemented "%s" for this operand.', [fun.name]);
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
  xLex.AddSymbSpec('-=', tnOperator);
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
  xLex.AddSymbSpec('^', tnOperator);
  xLex.AddSymbSpec('@', tnOperator);
  xLex.AddSymbSpec('.', tnOperator);
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
  opr.CreateOperation(typBit, @ROB_bit_asig_bit);
  opr.CreateOperation(typByte, @ROB_bit_asig_byte);

  opr:=typBit.CreateUnaryPreOperator('NOT', 6, 'not', @ROU_not_bit);

  opr:=typBit.CreateBinaryOperator('AND',4,'and');
  opr.CreateOperation(typBit,@ROB_bit_and_bit);
  opr.CreateOperation(typByte,@ROB_bit_and_byte);

  opr:=typBit.CreateBinaryOperator('OR',4,'or');
  opr.CreateOperation(typBit,@ROB_bit_or_bit);
  opr.CreateOperation(typByte,@ROB_bit_or_byte);

  opr:=typBit.CreateBinaryOperator('XOR',4,'or');
  opr.CreateOperation(typBit,@ROB_bit_xor_bit);
  opr.CreateOperation(typByte,@ROB_bit_xor_byte);

  opr:=typBit.CreateBinaryOperator('=',4,'equal');
  opr.CreateOperation(typBit,@ROB_bit_equ_bit);
  opr.CreateOperation(typByte,@ROB_bit_equ_byte);

  opr:=typBit.CreateBinaryOperator('<>',4,'difer');
  opr.CreateOperation(typBit,@ROB_bit_dif_bit);
  opr.CreateOperation(typByte,@ROB_bit_dif_byte);

  //////////////////////////////////////////
  //////// Operaciones con Boolean ////////////
  opr:=typBool.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typBool,@ROB_bool_asig_bool);

  opr:=typBool.CreateUnaryPreOperator('NOT', 6, 'not', @ROU_not_bool);

  opr:=typBool.CreateBinaryOperator('AND',4,'and');  //suma
  opr.CreateOperation(typBool,@ROB_bool_and_bool);

  opr:=typBool.CreateBinaryOperator('OR',4,'or');  //suma
  opr.CreateOperation(typBool,@ROB_bool_or_bool);

  opr:=typBool.CreateBinaryOperator('XOR',4,'or');  //suma
  opr.CreateOperation(typBool,@ROB_bool_xor_bool);

  opr:=typBool.CreateBinaryOperator('=',4,'equal');
  opr.CreateOperation(typBool,@ROB_bool_equ_bool);

  opr:=typBool.CreateBinaryOperator('<>',4,'difer');
  opr.CreateOperation(typBool,@ROB_bool_dif_bool);
  //////////////////////////////////////////
  //////// Operaciones con Byte ////////////
  //////////////////////////////////////////
  {Los operadores deben crearse con su precedencia correcta}
  opr:=typByte.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typByte,@ROB_byte_asig_byte);
  opr:=typByte.CreateBinaryOperator('+=',2,'aadd');  //asignación-suma
  opr.CreateOperation(typByte,@ROB_byte_aadd_byte);
  opr:=typByte.CreateBinaryOperator('-=',2,'asub');  //asignación-resta
  opr.CreateOperation(typByte,@ROB_byte_asub_byte);

  opr:=typByte.CreateBinaryOperator('+',4,'add');  //suma
  opr.CreateOperation(typByte,@ROB_byte_add_byte);
  opr.CreateOperation(typWord,@ROB_byte_add_word);
  opr:=typByte.CreateBinaryOperator('-',4,'subs');  //suma
  opr.CreateOperation(typByte,@ROB_byte_sub_byte);
  opr:=typByte.CreateBinaryOperator('*',5,'mult');  //byte*byte -> word
  opr.CreateOperation(typByte,@ROB_byte_mul_byte);
  opr:=typByte.CreateBinaryOperator('DIV',5,'div');  //byte / byte ->byte
  opr.CreateOperation(typByte,@ROB_byte_div_byte);
  opr:=typByte.CreateBinaryOperator('MOD',5,'mod');  //byte mod byte ->byte
  opr.CreateOperation(typByte,@ROB_byte_mod_byte);

  opr:=typByte.CreateBinaryOperator('AND',5,'and');  //suma
  opr.CreateOperation(typByte,@ROB_byte_and_byte);
  opr.CreateOperation(typBit ,@ROB_byte_and_bit);
  opr:=typByte.CreateBinaryOperator('OR',4,'or');  //suma
  opr.CreateOperation(typByte,@ROB_byte_or_byte);
  opr.CreateOperation(typBit,@ROB_byte_or_bit);
  opr:=typByte.CreateBinaryOperator('XOR',4,'xor');  //suma
  opr.CreateOperation(typByte,@ROB_byte_xor_byte);
  opr.CreateOperation(typBit,@ROB_byte_xor_bit);

  opr:=typByte.CreateUnaryPreOperator('NOT', 6, 'not', @ROU_not_byte);
  opr:=typByte.CreateUnaryPreOperator('@', 6, 'addr', @ROU_addr_byte);

  opr:=typByte.CreateBinaryOperator('=',3,'equal');
  opr.CreateOperation(typByte,@ROB_byte_equal_byte);
  opr:=typByte.CreateBinaryOperator('<>',3,'difer');
  opr.CreateOperation(typByte,@ROB_byte_difer_byte);
  opr.CreateOperation(typBit,@ROB_byte_difer_bit);

  opr:=typByte.CreateBinaryOperator('>',3,'great');
  opr.CreateOperation(typByte,@ROB_byte_great_byte);
  opr:=typByte.CreateBinaryOperator('<',3,'less');
  opr.CreateOperation(typByte,@ROB_byte_less_byte);

  opr:=typByte.CreateBinaryOperator('>=',3,'gequ');
  opr.CreateOperation(typByte,@ROB_byte_gequ_byte);
  opr:=typByte.CreateBinaryOperator('<=',3,'lequ');
  opr.CreateOperation(typByte,@ROB_byte_lequ_byte);

  opr:=typByte.CreateBinaryOperator('>>',5,'shr');  { TODO : Definir bien la precedencia }
  opr.CreateOperation(typByte,@ROB_byte_shr_byte);
  opr:=typByte.CreateBinaryOperator('<<',5,'shl');
  opr.CreateOperation(typByte,@ROB_byte_shl_byte);
  //////////////////////////////////////////
  //////// Operaciones con Char ////////////
  {Los operadores deben crearse con su precedencia correcta}
  opr:=typChar.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typChar,@ROB_char_asig_char);
  opr:=typChar.CreateBinaryOperator('=',3,'equal');  //asignación
  opr.CreateOperation(typChar,@ROB_char_equal_char);
  opr:=typChar.CreateBinaryOperator('<>',3,'difer');  //asignación
  opr.CreateOperation(typChar,@ROB_char_difer_char);

  //////////////////////////////////////////
  //////// Operaciones con Word ////////////
  {Los operadores deben crearse con su precedencia correcta}

  opr:=typWord.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typWord,@ROB_word_asig_word);
  opr.CreateOperation(typByte,@ROB_word_asig_byte);

  opr:=typWord.CreateBinaryOperator('=',3,'equal');  //igualdad
  opr.CreateOperation(typWord,@ROB_word_equal_word);
  opr:=typWord.CreateBinaryOperator('<>',3,'difer');
  opr.CreateOperation(typWord,@ROB_word_difer_word);
  opr:=typWord.CreateBinaryOperator('>',3,'difer');
  opr.CreateOperation(typWord,@ROB_word_great_word);

  opr:=typWord.CreateBinaryOperator('+',4,'suma');  //suma
  opr.CreateOperation(typWord,@ROB_word_add_word);
  opr.CreateOperation(typByte,@ROB_word_add_byte);

  opr:=typWord.CreateBinaryOperator('-',4,'subs');  //suma
  opr.CreateOperation(typWord,@ROB_word_sub_word);

  opr:=typWord.CreateBinaryOperator('AND', 5, 'and');  //AND
  opr.CreateOperation(typByte, @ROB_word_and_byte);

  opr:=typWord.CreateBinaryOperator('UMULWORD',5,'umulword');  //suma
  opr.CreateOperation(typWord,@ROB_word_umulword_word);

  opr:=typWord.CreateUnaryPreOperator('@', 6, 'addr', @ROU_addr_word);

  //////////////////////////////////////////
  //////// Operaciones con DWord ////////////
  {Los operadores deben crearse con su precedencia correcta}
  opr:=typDWord.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typDWord,@ROB_dword_asig_dword);
  opr.CreateOperation(typWord,@ROB_dword_asig_word);
  opr.CreateOperation(typByte,@ROB_dword_asig_byte);

  opr:=typDWord.CreateBinaryOperator('=',3,'equal');  //igualdad
  opr.CreateOperation(typDWord,@ROB_dword_equal_dword);
  opr:=typDWord.CreateBinaryOperator('<>',3,'difer');
  opr.CreateOperation(typDWord,@ROB_dword_difer_dword);

  opr:=typDWord.CreateBinaryOperator('+=',2,'asuma');  //suma
  opr.CreateOperation(typDWord,@ROB_dword_aadd_dword);

  opr:=typDWord.CreateBinaryOperator('+',4,'suma');  //suma
  opr.CreateOperation(typDWord,@ROB_dword_add_dword);
//  opr.CreateOperation(typByte,@ROB_word_add_byte);

end;
procedure TGenCod.DefPointerArithmetic(etyp: TxpEleType);
{Configura ls operaciones que definen la aritmética de punteros.}
var
  opr: TxpOperator;
begin
  //Asignación desde Byte y Puntero
  opr:=etyp.CreateBinaryOperator(':=',2,'asig');
  opr.CreateOperation(typByte, @ROB_byte_asig_byte);
  opr.CreateOperation(etyp   , @ROB_byte_asig_byte);
  //Agrega a los bytes, la posibilidad de ser asignados por punteros
  typByte.operAsign.CreateOperation(etyp, @ROB_byte_asig_byte);

  opr:=etyp.CreateBinaryOperator('=',3,'equal');  //asignación
  opr.CreateOperation(typByte, @ROB_byte_equal_byte);
  opr:=etyp.CreateBinaryOperator('+',4,'add');  //suma
  opr.CreateOperation(typByte, @ROB_pointer_add_byte);
  opr:=etyp.CreateBinaryOperator('-',4,'add');  //resta
  opr.CreateOperation(typByte, @ROB_pointer_sub_byte);
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
  //Funciones INLINE
  f := CreateSysFunction('exit'     , nil, @fun_Exit);
  f := CreateSysFunction('Inc'      , nil, @fun_Inc);
  f := CreateSysFunction('Dec'      , nil, @fun_Dec);
  f := CreateSysFunction('Ord'      , @callParam, @fun_Ord);
  f := CreateSysFunction('Chr'      , @callParam, @fun_Chr);
  f := CreateSysFunction('Bit'      , @callParam, @fun_Bit);
  f := CreateSysFunction('Boolean'  , @callParam, @fun_Bool);
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
  f_byte_mul_byte_16.compile := @mul_byte_16;
  //Multiplicación byte DIV, MOD byte a byte
  f_byte_div_byte := CreateSysFunction('byte_div_byte', nil, nil);
  f_byte_div_byte.adrr:=$0;
  f_byte_div_byte.compile := @byte_div_byte;
  //Multiplicación word por word a word
  f_word_mul_word_16 := CreateSysFunction('word_mul_word_16', nil, nil);
  f_word_mul_word_16.adrr:=$0;
  f_word_mul_word_16.compile := @word_mul_word_16;
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

