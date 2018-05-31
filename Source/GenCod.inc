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
    genError('Cannot Compile: "%s"', [OperationStr(Opt)]);
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
    genError('Cannot Compile: "%s"', [OperationStr(Opt)]);
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
      genError('Cannot Compile: "%s"', [OperationStr(Opt)]);
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
      genError('Cannot Compile: "%s"', [OperationStr(Opt)]);
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
      genError('Cannot Compile: "%s"', [OperationStr(Opt)]);
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
    kCOMF(byte1, toW);
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
////////////operaciones con Byte
procedure TGenCod.ROB_byte_and_byte(Opt: TxpOperation; SetRes: boolean);
var
  rVar: TxpEleVar;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;

  case stoOperation of
  stConst_Const: begin  //suma de dos constantes. Caso especial
    SetROBResultConst_byte(value1 and value2);  //puede generar error
  end;
  stConst_Variab: begin
    if value1 = 0 then begin  //Caso especial
      SetROBResultConst_byte(0);  //puede generar error
      exit;
    end else if value1 = 255 then begin  //Caso especial
      SetROBResultVariab(p2^.rVar);  //puede generar error
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    kMOVF(byte2, toW);
    kANDLW(value1);  //leave in W
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    if value1 = 0 then begin  //Caso especial
      SetROBResultConst_byte(0);  //puede generar error
      exit;
    end else if value1 = 255 then begin  //Caso especial
      SetROBResultExpres_byte(Opt);  //No es necesario hacer nada. Ya está en W
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    kANDLW(value1);  //leave in W
  end;
  stVariab_Const: begin
    if value2 = 0 then begin  //Caso especial
      SetROBResultConst_byte(0);  //puede generar error
      exit;
    end else if value1 = 255 then begin  //Caso especial
      SetROBResultVariab(p1^.rVar);  //puede generar error
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    kMOVLW(value2);
    kANDWF(byte1, toW);   //leave in W
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);
    kMOVF(byte2, toW);
    kANDWF(byte1, toW);   //leave in W
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    kANDWF(byte1, toW);   //leave in W
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    if value2 = 0 then begin  //Caso especial
      SetROBResultConst_byte(0);  //puede generar error
      exit;
    end else if value1 = 255 then begin  //Caso especial
      SetROBResultExpres_byte(Opt);  //No es necesario hacer nada. Ya está en W
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    kANDLW(value2);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    kANDWF(byte2, toW);
  end;
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
    //p1 está en la pila y p2 en el acumulador
    rVar := GetVarByteFromStk;
    kANDWF(rVar.adrByte0, toW);
    FreeStkRegisterByte;   //libera pila porque ya se uso
  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_byte_and_bit(Opt: TxpOperation; SetRes: boolean);
begin
  {No hay problema en usar siempre ExchangeP1_P2, porque el caso Expresión-Expresión,
  no se implementa ROB_bit_and_byte.}
  ExchangeP1_P2;   //Invierte los operandos
  ROB_bit_and_byte(Opt, SetRes);
end;
procedure TGenCod.ROB_byte_or_byte(Opt: TxpOperation; SetRes: boolean);
var
  rVar: TxpEleVar;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;

  case stoOperation of
  stConst_Const: begin  //suma de dos constantes. Caso especial
    SetROBResultConst_byte(value1 or value2);  //puede generar error
  end;
  stConst_Variab: begin
    if value1 = 0 then begin  //Caso especial
      SetROBResultVariab(p2^.rVar);
      exit;
    end else if value1 = 255 then begin  //Caso especial
      SetROBResultConst_byte(255);
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    kMOVF(byte2, toW);
    kIORLW(value1);  //leave in W
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    if value1 = 0 then begin  //Caso especial
      SetROBResultExpres_byte(Opt);  //No es necesario hacer nada. Ya está en W
      exit;
    end else if value1 = 255 then begin  //Caso especial
      SetROBResultConst_byte(255);
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    kIORLW(value1);  //leave in W
  end;
  stVariab_Const: begin
    if value2 = 0 then begin  //Caso especial
      SetROBResultVariab(p1^.rVar);
      exit;
    end else if value1 = 255 then begin  //Caso especial
      SetROBResultConst_byte(255);
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    kMOVLW(value2);
    kIORWF(byte1, toW);   //leave in W
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);
    kMOVF(byte2, toW);
    kIORWF(byte1, toW);   //leave in W
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    kIORWF(byte1, toW);   //leave in W
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    if value2 = 0 then begin  //Caso especial
      SetROBResultExpres_byte(Opt);  //No es necesario hacer nada. Ya está en W
      exit;
    end else if value2 = 255 then begin  //Caso especial
      SetROBResultConst_byte(255);
      exit;
    end;
    SetROBResultExpres_byte(Opt);
    kIORLW(value2);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    kIORWF(byte2, toW);
  end;
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
    //p1 está en la pila y p2 en el acumulador
    rVar := GetVarByteFromStk;
    kIORWF(rVar.adrByte0, toW);
    FreeStkRegisterByte;   //libera pila porque ya se uso
  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
end;
procedure TGenCod.ROB_byte_or_bit(Opt: TxpOperation; SetRes: boolean);
begin
  {No hay problema en usar siempre ExchangeP1_P2, porque el caso Expresión-Expresión,
  no se implementa ROB_bit_or_byte.}
  ExchangeP1_P2;   //Invierte los operandos
  ROB_bit_or_byte(Opt, SetRes);
end;
procedure TGenCod.ROB_byte_xor_byte(Opt: TxpOperation; SetRes: boolean);
var
  rVar: TxpEleVar;
begin
  if (p1^.Sto = stVarRefExp) and (p2^.Sto = stVarRefExp) then begin
    GenError('Too complex pointer expression.'); exit;
  end;
  if not ChangePointerToExpres(p1^) then exit;
  if not ChangePointerToExpres(p2^) then exit;

  case stoOperation of
  stConst_Const: begin  //suma de dos constantes. Caso especial
    SetROBResultConst_byte(value1 xor value2);  //puede generar error
  end;
  stConst_Variab: begin
    SetROBResultExpres_byte(Opt);
    kMOVF(byte2, toW);
    kXORLW(value1);  //leave in W
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    kXORLW(value1);  //leave in W
  end;
  stVariab_Const: begin
    SetROBResultExpres_byte(Opt);
    kMOVLW(value2);
    kXORWF(byte1, toW);   //leave in W
  end;
  stVariab_Variab:begin
    SetROBResultExpres_byte(Opt);
    kMOVF(byte2, toW);
    kXORWF(byte1, toW);   //leave in W
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    kXORWF(byte1, toW);   //leave in W
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    kXORLW(value2);
  end;
  stExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_byte(Opt);
    kXORWF(byte2, toW);
  end;
  stExpres_Expres:begin
    SetROBResultExpres_byte(Opt);
    //p1 está en la pila y p2 en el acumulador
    rVar := GetVarByteFromStk;
    kXORWF(rVar.adrByte0, toW);
    FreeStkRegisterByte;   //libera pila porque ya se uso
  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
  end;
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
    SetROBResultConst_bool(value1 = value2);
  end;
  stConst_Variab: begin
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    if value1 = 0 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _MOVF(p2^.offs, toF);  //si iguales _Z=1
    end else if value1 = 1 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _DECF(p2^.offs, toW);  //si el resultado es cero _Z=1
    end else if value1 = 255 then begin  //caso especial
      _BANKSEL(p2^.bank);  //verifica banco destino
      _INCF(p2^.offs, toW);  //si el resultado es cero _Z=1
    end else begin
      _MOVLW(value1);
      _BANKSEL(p2^.bank);  //verifica banco destino
      _SUBWF(p2^.offs, toW);  //si iguales _Z=1
    end;
  end;
  stConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    kXORLW(value1);  //Si son iguales Z=1. Se usa XORLW, porque SUBLW no existe en la gama baja
  end;
  stVariab_Const: begin
    ExchangeP1_P2;  //Convierte a stConst_Variab
    ROB_byte_equal_byte(Opt, SetRes);
  end;
  stVariab_Variab:begin
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    kMOVF(byte1, toW);
    kSUBWF(byte2, toW);  //si iguales _Z=1
  end;
  stVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    kSUBWF(byte1, toW);  //si iguales _Z=1
  end;
  stExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetROBResultExpres_bool(Opt, false);   //Se pide Z para el resultado
    kXORLW(value2);  //Si son iguales Z=1. Se usa XORLW, porque SUBLW no existe en la gama baja
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
    kSUBWF(rVar.adrByte0, toW);  //compara directamente a lo que había en pila.
    FreeStkRegisterByte;   //libera pila porque se usará el dato ahí contenido
  end;
  else
    genError(MSG_CANNOT_COMPL, [OperationStr(Opt)]);
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

