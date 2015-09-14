{Unidad con rutinas del analizador sintáctico.
}
unit Parser;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LCLType, Dialogs, lclProc, Graphics, SynEditHighlighter,
  SynFacilBasic, SynFacilHighlighter, SynFacilUtils, MisUtils, XpresBas,
  XpresParserPIC, Pic16Utils, PIC16devices, Globales, ProcAsm, types;
type

 { TCompiler }
  TCompiler = class(TCompilerBase)
  private
    //campos para controlar la codificación de rutinas iniciales
    iniBloSub  : integer;   //inicio del blqoue de subrutinas
    curBloSub  : integer;   //fin del bloque de subrutinas
    finBloSub  : integer;   //tamaño máximo del bloque de subrutinas
    iFlashTmp  : integer;   //almacenamiento temporal para pic.iFlash
    Traslape   : boolean;   //bandera de traslape
    ////////////////////////////////////////
    lexDir : TSynFacilSyn;  //lexer para analizar directivas
    pic : TPIC16;   //objeto PIC de la serie 16
    //Atributos adicionales
    tkType     : TSynHighlighterAttributes;
    tkStruct   : TSynHighlighterAttributes;
    tkDirective: TSynHighlighterAttributes;
    tkAsm      : TSynHighlighterAttributes;
    tkExpDelim : TSynHighlighterAttributes;
    tkBlkDelim : TSynHighlighterAttributes;
    tkOthers   : TSynHighlighterAttributes;
    procedure CaptureDecParams(f: integer);
    procedure CompileConstDeclar;
    procedure CompileIF;
    procedure CompileProcDeclar;
    procedure CompileREPEAT;
    procedure CompileWHILE;
    procedure CompileInstructionDummy;
    procedure CompileInstruction;
    function CreateCons(const consName: string; typ: ttype): integer;
    function CreateVar(const varName: string; typ: ttype; absAdd: integer=-1;
      absBit: integer=-1): integer;
    function CreateVar(varName, varType: string; absAdd: integer=-1; absBit: integer
      =-1): integer;
    procedure DefLexDir;
    procedure getListOfIdent(var itemList: TStringDynArray);
    procedure ProcComments;
    procedure CompileCurBlock;
    procedure CompilarArc(iniMem: word);
    procedure CompileVarDeclar;
  protected
    procedure TipDefecNumber(var Op: TOperand; toknum: string); override;
    procedure TipDefecString(var Op: TOperand; tokcad: string); override;
    procedure TipDefecBoolean(var Op: TOperand; tokcad: string); override;
    procedure cInNewLine(lin: string);
  public
    procedure StartSyntax;
    procedure Compilar(NombArc: string; LinArc: Tstrings);
    function RAMusage: string;  //uso de memoria RAM
    procedure DumpCode(l: TSTrings);  //uso de la memoria Flash
    procedure DumpStatistics(l: TSTrings);
    constructor Create; override;
    destructor Destroy; override;
  end;

//procedure Compilar(NombArc: string; LinArc: Tstrings);
var
  cxp : TCompiler;

implementation
//Variables que deden ser accesibles al generador de código
//var
//  i_w2 : integer;  //índice a la variable temporal byte
//  i_w3 : integer;  //índice a la variable temporal byte
//  i_w4 : integer;  //índice a la variable temporal byte
//Funciones de acceso rápido a métodos del compilador. Se usan para ayudar al geenrador de código.
//rutinas generales para la codificación
procedure CodAsmFD(const inst: TPIC16Inst; const f: byte; d: TPIC16destin); inline;
begin
  cxp.pic.codAsmFD(inst, f, d);
end;
procedure CodAsmK(const inst: TPIC16Inst; const k: byte); inline;
begin
  cxp.pic.codAsmK(inst, k);
end;
{procedure CodAsm(const inst: TPIC16Inst; const f, b: byte); inline;
begin
  cxp.pic.codAsmFB(inst, f, b);
end;}
//rutinas que facilitan la codifición de instrucciones
procedure _ADDWF(const f: byte; d: TPIC16destin); inline;
begin
  cxp.pic.codAsmFD(ADDWF, f,d);
end;
procedure _ANDWF(const f: byte; d: TPIC16destin); inline;
begin
  cxp.pic.codAsmFD(ANDWF, f,d);
end;
procedure _CLRF(const f: byte); inline;
begin
  cxp.pic.codAsmF(CLRF, f);
end;
procedure _CLRW(); inline;
begin
  cxp.pic.codAsm(CLRW);
end;
procedure _COMF(const f: byte; d: TPIC16destin); inline;
begin
  cxp.pic.codAsmFD(COMF, f,d);
end;
procedure _DECF(const f: byte; d: TPIC16destin); inline;
begin
  cxp.pic.codAsmFD(DECF, f,d);
end;
procedure _DECFSZ(const f: byte; d: TPIC16destin); inline;
begin
  cxp.pic.codAsmFD(DECFSZ, f,d);
end;
procedure _INCF(const f: byte; d: TPIC16destin); inline;
begin
  cxp.pic.codAsmFD(INCF, f,d);
end;
procedure _INCFSZ(const f: byte; d: TPIC16destin); inline;
begin
  cxp.pic.codAsmFD(INCFSZ, f,d);
end;
procedure _IORWF(const f: byte; d: TPIC16destin); inline;
begin
  cxp.pic.codAsmFD(IORWF, f,d);
end;
procedure _MOVF(const f: byte; d: TPIC16destin); inline;
begin
  cxp.pic.codAsmFD(MOVF, f,d);
end;
procedure _MOVWF(const f: byte); inline;
begin
  cxp.pic.codAsmF(MOVWF, f);
end;
procedure _NOP(); inline;
begin
  cxp.pic.codAsm(NOP);
end;
procedure _RLF(const f: byte; d: TPIC16destin); inline;
begin
  cxp.pic.codAsmFD(RLF, f,d);
end;
procedure _RRF(const f: byte; d: TPIC16destin); inline;
begin
  cxp.pic.codAsmFD(RRF, f,d);
end;
procedure _SUBWF(const f: byte; d: TPIC16destin); inline;
begin
  cxp.pic.codAsmFD(SUBWF, f,d);
end;
procedure _SWAPF(const f: byte; d: TPIC16destin); inline;
begin
  cxp.pic.codAsmFD(SWAPF, f,d);
end;
procedure _XORWF(const f: byte; d: TPIC16destin); inline;
begin
  cxp.pic.codAsmFD(XORWF, f,d);
end;
procedure _BCF(const f, b: byte); inline;
begin
  cxp.pic.codAsmFB(BCF, f, b);
end;
procedure _BSF(const f, b: byte); inline;
begin
  cxp.pic.codAsmFB(BSF, f, b);
end;
procedure _BTFSC(const f, b: byte); inline;
begin
  cxp.pic.codAsmFB(BTFSC, f, b);
end;
procedure _BTFSS(const f, b: byte); inline;
begin
  cxp.pic.codAsmFB(BTFSS, f, b);
end;
procedure _ADDLW(const k: word); inline;
begin
  cxp.pic.codAsmK(ADDLW, k);
end;
procedure _ANDLW(const k: word); inline;
begin
  cxp.pic.codAsmK(ANDLW, k);
end;
procedure _CALL(const a: word); inline;
begin
  cxp.pic.codAsmA(CALL, a);
end;
procedure _CLRWDT(); inline;
begin
  cxp.pic.codAsm(CLRWDT);
end;
procedure _GOTO(const a: word); inline;
begin
  cxp.pic.codAsmA(GOTO_, a);
end;
procedure _IORLW(const k: word); inline;
begin
  cxp.pic.codAsmK(IORLW, k);
end;
procedure _MOVLW(const k: word); inline;
begin
  cxp.pic.codAsmK(MOVLW, k);
end;
procedure _RETFIE(); inline;
begin
  cxp.pic.codAsm(RETFIE);
end;
procedure _RETLW(const k: word); inline;
begin
  cxp.pic.codAsmK(RETLW, k);
end;
procedure _RETURN(); inline;
begin
  cxp.pic.codAsm(RETURN);
end;
procedure _SLEEP(); inline;
begin
  cxp.pic.codAsm(SLEEP);
end;
procedure _SUBLW(const k: word); inline;
begin
  cxp.pic.codAsmK(SUBLW, k);
end;
procedure _XORLW(const k: word); inline;
begin
  cxp.pic.codAsmK(XORLW, k);
end;
procedure _GOTO_PEND(var  igot: integer);
{Escribe una instrucción GOTO, pero sin precisar el destino aún. Devuelve la dirección
 donde se escribe el GOTO, para poder completarla posteriormente.
}
begin
  igot := cxp.pic.iFlash;  //guarda posición de instrucción de salto
  cxp.pic.codAsmA(GOTO_, 0);  //pone salto indefinido
end;
function _PC: word; inline;
{Devuelve la dirección actual en Flash}
begin
  Result := cxp.pic.iFlash;
end;
function _CLOCK: integer; inline;
{Devuelve la frecuencia de reloj del PIC}
begin
  Result := cxp.pic.frequen;
end;
procedure PutComLine(cmt: string); inline; //agrega comentario al código
begin
  cxp.pic.addCommAsm(cmt);  //agrega línea al código ensmblador
end;
procedure PutComm(cmt: string); inline; //agrega comentario lateral al código
begin
  cxp.pic.addCommAsm1('|'+cmt);  //agrega línea al código ensmblador
end;
procedure StartCodeSub(ifun: integer);
{debe ser llamado para iniciar la codificación de una subrutina}
begin
  cxp.iFlashTmp :=  cxp.pic.iFlash;     //guarda puntero
  cxp.pic.iFlash := cxp.curBloSub;  //empieza a codificar aquí
  funcs[ifun].adrr := cxp.curBloSub;  //fija inicio de rutina
end;
procedure EndCodeSub;
{debe ser llamado al terminar la codificaión de una subrutina}
begin
  cxp.curBloSub := cxp.pic.iFlash;  //indica siguiente posición libre
  if cxp.curBloSub > cxp.finBloSub then begin
    //hubo traslape
    cxp.Traslape:=true;
  end;
  cxp.pic.iFlash := cxp.iFlashTmp;     //retorna puntero
end;
procedure callFunct(ifun: integer);
{Rutina que debe llamara a uan función definida por el usuario}
begin
  //por ahora no hay problema de paginación
  _CALL(funcs[ifun].adrr);
end;

procedure GenError(msg: string);
begin
  cxp.GenError(msg);
end;
procedure GenError(msg: string; const Args: array of const);
begin
  cxp.GenError(msg,Args);
end;
function HayError: boolean;
begin
  Result := cxp.HayError;
end;
function CreateVar(const varName: string; typ: ttype): integer;
begin
  Result := cxp.CreateVar(varName, typ);
end;
{Incluye el código del compilador. Aquí tendrá acceso a todas las variables públicas
 de XPresParser}
{$I GenCod.pas}
//Métodos OVERRIDE
procedure TCompiler.TipDefecNumber(var Op: TOperand; toknum: string);
{Procesa constantes numéricas, ubicándolas en el tipo de dato apropiado (byte, word, ... )
 Si no logra ubicar el tipo de número, o no puede leer su valro, generará  un error.}
var
  n: int64;   //para almacenar a los enteros
  f: extended;  //para almacenar a reales
  i: Integer;
  menor: Integer;
  imen: integer;
begin
  if pos('.',toknum) <> 0 then begin  //es flotante
    Op.catTyp := t_float;   //es flotante
    try
      f := StrToFloat(toknum);  //carga con la mayor precisión posible
    except
      Op.typ := nil;
      GenError('Unvalid float number.');
      exit;
    end;
    //busca el tipo numérico más pequeño que pueda albergar a este número
    Op.size := 4;   //se asume que con 4 bytes bastará
    {Aquí se puede decidir el tamaño de acuerdo a la cantidad de decimales indicados}

    Op.valFloat := f;  //debe devolver un extended
    menor := 1000;
    for i:=0 to typs.Count-1 do begin
      { TODO : Se debería tener una lista adicional TFloatTypes, para acelerar la
      búsqueda}
      if (typs[i].cat = t_float) and (typs[i].size>=Op.size) then begin
        //guarda el menor
        if typs[i].size < menor then  begin
           imen := i;   //guarda referencia
           menor := typs[i].size;
        end;
      end;
    end;
    if menor = 1000 then  //no hubo tipo
      Op.typ := nil
    else  //encontró
      Op.typ:=typs[imen];

  end else begin     //es entero
    Op.catTyp := t_uinteger;   //es entero sin signo
    //verificación de longitud de entero
    if toknum[1] = '$' then begin
      //formato hexadecimal
      { TODO : Una verificación más precisa debería considerar hasta 64 bits y quitar los posibles ceros delante o intentar convertir y capturar el error. }
      if length(toknum)>9 then begin  //protege antes de intentar convertirlo
        GenError('Very large number. Cannot process.');
      end;
    end else if toknum[1] = '%' then begin
      //formato binario
      if length(toknum)>33 then begin  //protege antes de intentar convertirlo
        GenError('Very large number. Cannot process.');
      end;
    end else begin
      //formato decimal
      if length(toknum)>=19 then begin  //solo aquí puede haber problemas
        if toknum[1] = '-' then begin  //es negativo
          if length(toknum)>20 then begin
            GenError('Very large number. Cannot process.');
            exit
          end else if (length(toknum) = 20) and  (toknum > '-9223372036854775808') then begin
            GenError('Very large number. Cannot process.');
            exit
          end;
        end else begin  //es positivo
          if length(toknum)>19 then begin
            GenError('Very large number. Cannot process.');
            exit
          end else if (length(toknum) = 19) and  (toknum > '9223372036854775807') then begin
            GenError('Very large number. Cannot process.');
            exit
          end;
        end;
      end;
    end;
    //conversión. aquí ya no debe haber posibilidad de error, excepto por tamaño
    n := StrToInt64(toknum);
    Op.valInt := n;   //copia valor de constante entera
    if (n>= 0) and  (n<=255) then begin
      Op.size := 1;
      Op.typ := tipByte;
    end else if (n>= 0) and  (n<=65535) then begin
      Op.size := 2;
      Op.typ := tipWord;
    end else  begin //no encontró
      GenError('No type defined to accommodate this number.');
      Op.typ := nil;
    end;
  end;
end;
procedure TCompiler.TipDefecString(var Op: TOperand; tokcad: string);
//Devuelve el tipo de cadena encontrado en un token
//var
//  i: Integer;
begin
{  Op.catTyp := t_string;   //es cadena
  Op.size:=length(tokcad);
  //toma el texto
  Op.valStr := copy(cIn.tok,2, length(cIn.tok)-2);   //quita comillas
  //////////// Verifica si hay tipos string definidos ////////////
  if length(Op.valStr)=1 then begin
    Op.typ := tipChr;
  end else
    Op.typ :=nil;  //no hay otro tipo}
end;
procedure TCompiler.TipDefecBoolean(var Op: TOperand; tokcad: string);
//Devuelve el tipo de cadena encontrado en un token
begin
  Op.catTyp := t_boolean;   //es flotante
  Op.size:=-1;   //se usará un byte
  //convierte valor constante
  Op.valBool:= (tokcad[1] in ['t','T']);
  Op.typ:=tipBool;
end;
procedure TCompiler.ProcComments;
//Procesa comentarios y directivas
  function tokType: TSynHighlighterAttributes;
begin
  Result := TSynHighlighterAttributes(lexdir.GetTokenKind);
end;
  procedure skipWhites;
begin
  if tokType = lexDir.tkSpace then
    lexDir.Next;  //quita espacios
end;
var
  f: Integer;
begin
  cIn.SkipWhites;
  while (cIn.tokType = tkDirective) or (cIn.tokType = tkAsm) do begin
    if cIn.tokType = tkAsm then begin
      //procesa la línea ASM
      ProcASMlime(cIn.tok);
      if HayError then exit;
    end else begin
      //Se ha detectado una directiva
      //Usa SynFacilSyn como lexer para analizar texto
      lexDir.SetLine(copy(Cin.tok,3,1000), 0);  //inicica cadena
      if tokType = lexDir.tkSpace then  lexDir.Next;  //quita espacios
      if tokType <> lexDir.tkIdentif then begin
        GenError('Error in directive.');
        exit;
      end;
      //sigue identificador
      case UpperCase(lexDir.GetToken) of
      'PROCESSOR': begin
        lexDir.Next;  //pasa al siguiente
        skipWhites;
        if not GetHardwareInfo(pic, lexDir.GetToken) then begin
          GenError('Unknown device: %s', [lexDir.GetToken]);
          exit;
        end;
      end;
      'FREQUENCY': begin
        lexDir.Next;  //pasa al siguiente
        skipWhites;
        if tokType <> lexDir.tkNumber then begin
          GenError('Error in directive.');
          exit;
        end;
        if not TryStrToInt(lexDir.GetToken, f) then begin
          GenError('Error in frecuencia.');
          exit;
        end;
        lexDir.Next;  //pasa al siguiente
        skipWhites;
        case UpperCase(lexDir.GetToken) of
        'KHZ': f := f * 1000;
        'MHZ': f := f * 1000000;
        else
          GenError('Error in directive.');
          exit;
        end;
        pic.frequen:=f; //asigna freecuencia
      end;
      'POINTERS': begin
        lexDir.Next;  //pasa al siguiente
        skipWhites;

      end;
      'CONFIG': begin
        lexDir.Next;  //pasa al siguiente
        skipWhites;

      end;
      'DEFINE': begin
        lexDir.Next;  //pasa al siguiente
        skipWhites;

      end;
      else
        GenError('Unknown directive: %s', [lexDir.GetToken]);
        exit;
      end;
    end;
    //pasa w siguiente
    cIn.Next;
    cIn.SkipWhites;  //limpia blancos
  end;
end;
procedure TCompiler.cInNewLine(lin: string);
//Se pasa a una nueva _Línea en el contexto de entrada
begin
  pic.addCommAsm(';'+lin);  //agrega _Línea al código ensmblador
end;
procedure TCompiler.getListOfIdent(var itemList: TStringDynArray);
{Lee una lista de identificadores separados por comas, hasta encontra un caracter distinto
de coma. Si el primer elemento no es un identificador o si después de la coma no sigue un
identificador, genera error.}
var
  item: String;
  n: Integer;
begin
  repeat
    cIn.SkipWhites;
    //ahora debe haber un identificador
    if cIn.tokType <> tkIdentif then begin
      GenError('Identifier expected.');
      exit;
    end;
    //hay un identificador
    item := cIn.tok;
    cIn.Next;  //lo toma
    cIn.SkipWhites;
    //sgrega nombre de ítem
    n := high(itemList)+1;
    setlength(itemList, n+1);  //hace espacio
    itemList[n] := item;  //agrega nombre
    if cIn.tok <> ',' then break; //sale
    cIn.Next;  //toma la coma
  until false;
end;
function TCompiler.CreateCons(const consName: string; typ: ttype): integer;
{Rutina para crear una constante. Devuelve índice a la variable creada.}
var
  r   : TCons;
  n, i: Integer;
  offs, bnk, bit : byte;
begin
  //verifica nombre
  if FindPredefName(consName) <> idtNone then begin
    GenError('Duplicated identifier: "%s"', [consName]);
    exit;
  end;
  //registra variable en la tabla
  r.nom:=consName;
  r.typ := typ;   //fija  referencia a tipo
  n := high(cons)+1;
  setlength(cons, n+1);
  cons[n] := r;
  Result := n;
end;
function TCompiler.CreateVar(const varName: string; typ: ttype;
         absAdd: integer = -1; absBit: integer = -1): integer;
{Rutina para crear variable. Devuelve índice a la variable creada. Si se especifican
 "absAdd" y/o "absBit", se coloca a la variable en una dirección absoluta.}
var
  r   : Tvar;
  n, i: Integer;
  offs, bnk, bit : byte;
begin
  //verifica nombre
  if FindPredefName(varName) <> idtNone then begin
    GenError('Duplicated identifier: "%s"', [varName]);
    exit;
  end;
  //registra variable en la tabla
  r.nom:=varName;
  r.typ := typ;   //fija  referencia a tipo
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
    //Es un boolean
    pic.SetNameRAM(offs,bnk, '_map');   //no tiene nombre único
  end else begin
    //Se asume que la variable ha sido entregada con posiciones consecutivas
    for i:=0 to typ.size -1 do
      pic.SetNameRAM(offs+i, bnk, varName+'['+IntToStr(i)+']');
  end;

  r.offs := offs;
  r.bank := bnk;
  r.bit  := bit;
  n := high(vars)+1;
  setlength(vars, n+1);
  vars[n] := r;
  Result := n;
  //Ya encontró tipo, llama a evento
  if typ.OnGlobalDef<>nil then typ.OnGlobalDef(varName, '');
end;
function TCompiler.CreateVar(varName, varType: string;
         absAdd: integer = -1; absBit: integer = -1): integer;
{Agrega una variable w la tabla de variables.}
var t: ttype;
  hay: Boolean;
  varTypeL: String;
begin
  //Verifica el tipo
  hay := false;
  varTypeL := LowerCase(varType);
  for t in typs do begin
    if t.name = varTypeL then begin
       hay:=true; break;
    end;
  end;
  if not hay then begin
    GenError('Undefined type "%s"', [varType]);
    exit;
  end;
  Result := CreateVar(varName, t, absAdd ,absBit);
  //puede salir con error
end;
procedure TCompiler.CompileVarDeclar;
{Compila la declaración de variables. Usa una sintaxis, sencilla, similar w la de
 Pascal. Lo normal seríw que se sobreescriba este método para adecuarlo al lenguaje
 que se desee implementar. }
var
  absAdrr: word;
  absBit: byte;
  Number: TOperand;  //para ser es usado por las subrutinas

  procedure CheckAbsoluteBit;
  {Extrae la parte del bit de una dirección absoluta. Actualiza "absBit"}
  begin
    if cIn.tok<>'.' then begin
      GenError('"." expected.');
      exit;
    end;
    cIn.Next;    //Pasa al siguiente
    //toma posición de bit
    TipDefecNumber(Number, cIn.tok); //encuentra tipo de número, tamaño y valor
    if pErr.HayError then exit;  //verifica
    if Number.CanBeByte then
       absBit := Number.valInt
    else begin
      GenError('Invalid memory address.');
      exit;
    end;
    if absBit > 7 then begin
      GenError('Invalid memory address.');
      exit;
    end;
    cIn.Next;    //Pasa al siguiente
  end;
  procedure CheckAbsolute(var IsAbs: boolean; const IsBoolean: boolean);
  {Verifica si lo que sigue es la sintaxis ABSOLUTE ... . Si esa así, procesa el texto,
  pone "IsAbs" en TRUE y actualiza los valores "absAdrr" y "absBit". }
  var
    ivar: integer;
    n: integer;
    tmp: String;
  begin
    IsAbs := false;   //bandera
    if (cIn.tokL <> 'absolute') and (cIn.tok <> '@') then
      exit;  //no es variable absoluta
    //// Hay especificación de dirección absoluta ////
    IsAbs := true;    //marca bandera
    cIn.Next;
    cIn.SkipWhites;
    if cIn.tokType = tkNumber then begin
      TipDefecNumber(Number, cIn.tok); //encuentra tipo de número, tamaño y valor
      if pErr.HayError then exit;  //verifica
      if Number.catTyp = t_float then begin
        //Caso especial porque el puede ser el formato <dirección>.<bit> que es
        //totalmenet válido, y el lexer lo captura como un solo token.
        if IsBoolean then begin
          //Puede ser válido el número "decimal", hay que extraer los campos,
          //pero primero hay que asegurarnos que no tenga notación exponencial.
          if (pos('e', cIn.tok)<>0) or (pos('E', cIn.tok)<>0) then begin
            GenError('Invalid memory address.');
            exit;
          end;
          //ya sabemos que tiene que ser decimal, con punto
          absAdrr := trunc(Number.valFloat);   //la dirección es la parte entera
          n := pos('.', cIn.tok);   //no debe fallar
          tmp := copy(cIn.tok, n+1, 1000);   //copia parte decimal
          if length(tmp)<>1 then begin  //valida longitud
            GenError('Invalid memory address.');
            exit;
          end;
          absBit:=StrToInt(tmp);   //no debe fallar
          //valida
          if not pic.ValidRAMaddr(absAdrr) then begin
            GenError('Invalid memory address for this device.');
            exit;
          end;
          if absBit > 7 then begin
            GenError('Invalid memory address.');
            exit;
          end;
          cIn.Next;    //Pasa al siguiente
        end else begin  //no puede ser correcto
          GenError('Invalid memory address.');
          exit;
        end;
      end else begin
        //Se asume número entero
        if Number.CanBeWord then
           absAdrr := Number.aWord
        else begin
          GenError('Invalid memory address.');
          exit;
        end;
        if not pic.ValidRAMaddr(absAdrr) then begin
          GenError('Invalid memory address for this device.');
          exit;
        end;
        cIn.Next;    //Pasa al siguiente
        if IsBoolean then begin
          CheckAbsoluteBit;  //es un boolean, debe especificarse el bit
          if pErr.HayError then exit;  //verifica
        end;
      end;
    end else if cIn.tokType = tkIdentif then begin
      //puede ser variable
      if FindVar(cIn.tok, ivar) then begin
        absAdrr:=vars[ivar].offs + vars[ivar].bank * $80;  //debe ser absoluta
      end else begin
        GenError('Identifier of variable expected.');
        exit;
      end;
      cIn.Next;    //Pasa al siguiente
      if IsBoolean then begin
        CheckAbsoluteBit;  //es un boolean, debe especificarse el bit
        if pErr.HayError then exit;  //verifica
      end;
    end else begin   //error
      GenError('Numeric address expected.');
      exit;
    end;
  end;

var
  varType: String;
  varNames: array of string;  //nombre de variables
  tmp: String;
  isAbsolute: Boolean;
begin
  setlength(varNames,0);  //inicia arreglo
  //procesa variables a,b,c : int;
  getListOfIdent(varNames);
  if HayError then begin  //precisa el error
    GenError('Identifier of variable expected.');
    exit;
  end;
  //usualmente debería seguir ":"
  if cIn.tok = ':' then begin
    //debe venir el tipo de la variable
    cIn.Next;  //lo toma
    cIn.SkipWhites;
    if (cIn.tokType <> tkType) then begin
      GenError('Identifier of type expected.');
      exit;
    end;
    varType := cIn.tok;   //lee tipo
    cIn.Next;
    cIn.SkipWhites;
    //verifica si tiene dirección absoluta
    CheckAbsolute(isAbsolute, LowerCase(varType) = 'boolean');
    if Perr.HayError then exit;
    //reserva espacio para las variables
    for tmp in varNames do begin
      if isAbsolute then  //crea en posición absoluta
        CreateVar(tmp, varType, absAdrr, absBit)
      else
        CreateVar(tmp, varType);
      if Perr.HayError then exit;
    end;
  end else begin
    GenError('":" or "," expected.');
    exit;
  end;
  if not CaptureDelExpres then exit;
  ProcComments;
  //puede salir con error
end;
procedure TCompiler.CompileConstDeclar;
var
//  consType: String;
  consNames: array of string;  //nombre de variables
  n: Integer;
  tmp: String;
begin
  setlength(consNames,0);  //inicia arreglo
  //procesa lista de constantes a,b,c ;
  getListOfIdent(consNames);
  if HayError then begin  //precisa el error
    GenError('Identifier of constant expected.');
    exit;
  end;
  //puede seguir "=" o identificador de tipo
  if cIn.tok = '=' then begin
    cIn.Next;  //pasa al siguiente
    //Debe seguir una expresión constante, que no genere código
    GetExpression(0);
    if HayError then exit;
    if res.catOp <> coConst then begin
      GenError('Constant expression expected.');
    end;
    //Hasta aquí todo bien, crea la(s) constante(s).
    for tmp in consNames do begin
      //crea constante
      n := CreateCons(tmp, res.typ);
      res.CopyConsValTo(cons[n]); //asigna valor
    end;
//  end else if cIn.tok = ':' then begin
  end else begin
    GenError('"=", ":" or "," expected.');
    exit;
  end;
  if not CaptureDelExpres then exit;
  ProcComments;
  //puede salir con error
end;
procedure TCompiler.CaptureDecParams(f: integer);
//Lee la declaración de parámetros de una función.
var
  parType: String;
  parName: String;
begin
  SkipWhites;
  ClearParamsFunc(0);   //inicia parámetros
  if EOBlock or EOExpres then begin
    //no tiene parámetros
  end else begin
    //Debe haber parámetros
    if cIn.tok<>'(' then begin
      GenError('"(" expected.');
      exit;
    end;
    cin.Next;
    repeat
      if cIn.tokType <> tkIdentif then begin
        GenError('Identifier expected.');
        exit;
      end;
      parName := cIn.tok;   //lee tipo de parámetro
      cIn.Next;
      cIn.SkipWhites;
      if cIn.tok<>':' then begin
        GenError('":" expected.');
        exit;
      end;
      cIn.Next;
      cIn.SkipWhites;

      if (cIn.tokType <> tkType) then begin
        GenError('Identifier of type expected.');
        exit;
      end;
      parType := cIn.tok;   //lee tipo de parámetro
      cIn.Next;
      //ya tiene el nombre y el tipo
      CreateParam(f, parName, parType);
      if HayError then exit;
      if cIn.tok = ';' then begin
        cIn.Next;   //toma separador
        SkipWhites;
      end else begin
        //no sigue separador de parámetros,
        //debe terminar la lista de parámetros
        //¿Verificar EOBlock or EOExpres ?
        break;
      end;
    until false;
    //busca paréntesis final
    if cIn.tok<>')' then begin
      GenError('")" expected.'); exit;
    end;
    cin.Next;
  end;
end;
procedure TCompiler.CompileProcDeclar;
{Compila la declaración de procedimientos. Tanto procedimeintos como funciones
 se manejan internamenet como funciones}
var
  procName: String;
  ifun: Integer;
begin
  cIn.SkipWhites;
  //ahora debe haber un identificador
  if cIn.tokType <> tkIdentif then begin
    GenError('Identifier expected.');
    exit;
  end;
  //hay un identificador
  procName := cIn.tok;
  cIn.Next;  //lo toma
  {Ya tiene los datos mínimos para crear la función. La crea con "proc" en NIL,
  para indicar que es una función definida por el usuario.}
  ifun := CreateFunction(procName, typNull, @callFunct);
  if HayError then exit;
  CaptureDecParams(ifun);
  cIn.SkipWhites;
  if cIn.tok=';' then begin //encontró delimitador de expresión
    cIn.Next;   //lo toma
    ProcComments;  //quita espacios
    if HayError then exit;   //puede dar error por código assembler o directivas
  end else begin  //hay otra cosa
    GenError('";" expected.');  //por ahora
    exit;  //debe ser un error
  end;
  //por ahora no se soporta variables locales
  if cIn.tokL <> 'begin' then begin
    GenError('"begin" expected.');
    exit;
  end;
  StartCodeSub(ifun);  //inicia codificación de subrutina
  CompileInstruction;
  _RETURN();  //instrucción de salida
  EndCodeSub;  //termina codificación
  if cIn.tokType=tkExpDelim then begin //encontró delimitador de expresión
    cIn.Next;   //lo toma
    ProcComments;  //quita espacios
    if HayError then exit;   //puede dar error por código assembler o directivas
  end else begin  //hay otra cosa
    GenError('";" expected.');  //por ahora
    exit;  //debe ser un error
  end;
end;
procedure TCompiler.CompileWHILE;
{Compila uan extructura WHILE}
var
  l1: Word;
  dg: Integer;
begin
  l1 := _PC;        //guarda dirección de inicio
  GetExpression(0);
  if HayError then exit;
  if res.typ<>tipBool then begin
    GenError('Boolean expression expected.');
    exit;
  end;
  cIn.SkipWhites;
  if cIn.tokL<>'do' then begin
    GenError('"do" expected.');
    exit;
  end;
  cIn.Next;   //toma "do"
  //aquí debe estar el cuerpo del "while"
  case res.catOp of
  coConst: begin  //la condición es fija
    if res.valBool then begin
      //lazo infinito
      CompileInstruction;  //debería completarse las instrucciones de tipo "break"
      if HayError then exit;
      _GOTO(l1);
    end else begin
      //lazo nulo
      //aquí se debería lanzar una advertencia
      CompileInstructionDummy;
      if HayError then exit;
    end;
  end;
  coVariab:begin
    _BTFSS(res.offs, res.bit);  //verifica condición
    _GOTO_PEND(dg);  //salto pendiente
    CompileInstruction;
    if HayError then exit;
    _GOTO(l1);
    //ya se tiene el destino del salto
    pic.codGotoAt(dg, _PC);   //termina de codificar el salto
  end;
  coExpres:begin
    if BooleanInverted then  //_Lógica invertida
      _BTFSC(_STATUS, BooleanBit)  //verifica condición
    else
      _BTFSS(_STATUS, BooleanBit);  //verifica condición
    _GOTO_PEND(dg);  //salto pendiente
    CompileInstruction;
    if HayError then exit;
    _GOTO(l1);
    //ya se tiene el destino del salto
    pic.codGotoAt(dg, _PC);   //termina de codificar el salto
  end;
  end;
end;
procedure TCompiler.CompileREPEAT;
{Compila uan extructura WHILE}
var
  l1: Word;
  dg: Integer;
begin
  l1 := _PC;        //guarda dirección de inicio
//  CompileInstruction;  //debería completarse las instrucciones de tipo "break"
  CompileCurBlock;
  if HayError then exit;
  cIn.SkipWhites;
  if cIn.tokL<>'until' then begin
    GenError('"until" expected.');
    exit;
  end;
  cIn.Next;   //toma "until"
  GetExpression(0);
  if HayError then exit;
  if res.typ<>tipBool then begin
    GenError('Boolean expression expected.');
    exit;
  end;
  case res.catOp of
  coConst: begin  //la condición es fija
    if res.valBool then begin
      //lazo nulo
    end else begin
      //lazo infinito
      _GOTO(l1);
    end;
  end;
  coVariab:begin
    _BTFSS(res.offs, res.bit);  //verifica condición
    _GOTO(l1);
    //sale cuando la condición es verdadera
  end;
  coExpres:begin
    if BooleanInverted then  //_Lógica invertida
      _BTFSC(_STATUS, BooleanBit)  //verifica condición
    else
      _BTFSS(_STATUS, BooleanBit);  //verifica condición
    _GOTO(l1);
    //sale cuando la condición es verdadera
  end;
  end;
end;
procedure TCompiler.CompileIF;
{Compila uan extructura IF}
var
  dg: Integer;
  dg2: Integer;
begin
  GetExpression(0);
  if HayError then exit;
  if res.typ<>tipBool then begin
    GenError('Boolean expression expected.');
    exit;
  end;
  cIn.SkipWhites;
  if cIn.tokL<>'then' then begin
    GenError('"then" expected.');
    exit;
  end;
  cIn.Next;   //toma "then"
  //aquí debe estar el cuerpo del "if"
  case res.catOp of
  coConst: begin  //la condición es fija
    if res.valBool then begin
      //es verdadero, siempre se ejecuta
      CompileInstruction;
      if HayError then exit;
      if cIn.tokL = 'else' then begin
        //hay bloque ELSE, pero no se ejecutará nunca
        cIn.Next;   //toma "else"
        CompileInstructionDummy;  //solo para mantener la sintaxis
        if HayError then exit;
      end;
    end else begin
      //aquí se debería lanzar una advertencia
      CompileInstructionDummy;  //solo para mantener la sintaxis
      if HayError then exit;
      if cIn.tokL = 'else' then begin
        //hay bloque ELSE, que sí se ejecutará
        cIn.Next;   //toma "else"
        CompileInstruction;
        if HayError then exit;
      end;
    end;
  end;
  coVariab:begin
    _BTFSS(res.offs, res.bit);  //verifica condición
    _GOTO_PEND(dg);  //salto pendiente
    CompileInstruction;
    if HayError then exit;
    if cIn.tokL <> 'else' then begin  //no hay blqoue ELSE
      pic.codGotoAt(dg, _PC);   //termina de codificar el salto
    end else begin
      //hay bloque ELSE
      cIn.Next;   //toma "else"
      _GOTO_PEND(dg2);  //salto pendiente
      pic.codGotoAt(dg, _PC);   //termina de codificar el salto
      CompileInstruction;
      if HayError then exit;
      pic.codGotoAt(dg2, _PC);   //termina de codificar el salto
    end;
  end;
  coExpres:begin
    if BooleanInverted then  //_Lógica invertida
      _BTFSC(_STATUS, BooleanBit)  //verifica condición
    else
      _BTFSS(_STATUS, BooleanBit);  //verifica condición
    _GOTO_PEND(dg);  //salto pendiente
    CompileInstruction;
    if HayError then exit;
    if cIn.tokL <> 'else' then begin  //no hay blqoue ELSE
      pic.codGotoAt(dg, _PC);   //termina de codificar el salto
    end else begin
      //hay bloque ELSE
      cIn.Next;   //toma "else"
      _GOTO_PEND(dg2);  //salto pendiente
      pic.codGotoAt(dg, _PC);   //termina de codificar el salto
      CompileInstruction;
      if HayError then exit;
      pic.codGotoAt(dg2, _PC);   //termina de codificar el salto
    end;
  end;
  end;
end;
procedure TCompiler.CompileInstructionDummy;
{Compila una instrucción pero sin generar código. }
var
  p: Integer;
begin
  p := pic.iFlash;
  CompileInstruction;  //compila solo para mantener la sintaxis
  pic.iFlash := p;     //elimina lo compilado
  //puede salir con error
  { TODO : Debe limpiar la memoria flash que ocupó, para dejar la casa limpia. }
end;
procedure TCompiler.CompileInstruction;
{Compila una única instrucción o un bloque BEGIN ... END}
begin
  ProcComments;
  if HayError then exit;   //puede dar error por código assembler o directivas
  if cIn.tokL='begin' then begin
    //es bloque
    cIn.Next;  //toma "begin"
    CompileCurBlock;   //llamada recursiva
    if HayError then exit;
    if cIn.tokL<>'end' then begin
      GenError('"end" expected.');
      exit;
    end;
    cIn.Next;  //toma "end"
    ProcComments;
    //puede salir con error
  end else begin
    //es una instrucción
    if cIn.tokType = tkStruct then begin
      if cIn.tokl = 'if' then begin
        cIn.Next;         //pasa "if"
        CompileIF;
      end else if cIn.tokl = 'while' then begin
        cIn.Next;         //pasa "while"
        CompileWHILE;
      end else if cIn.tokl = 'repeat' then begin
        cIn.Next;         //pasa "until"
        CompileREPEAT;
      end else begin
        GenError('Unknown structure.');
        exit;
      end;
    end else begin
      //debe ser es una expresión
      GetExpression(0);
    end;
  end;
end;
procedure TCompiler.CompileCurBlock;
{Compila el bloque de código actual hasta encontrar un delimitador de bloque, o fin
de archivo. }
begin
  ProcComments;
  if HayError then exit;   //puede dar error por código assembler o directivas
  while not cIn.Eof and (cIn.tokType<>tkBlkDelim) do begin
    //se espera una expresión o estructura
    CompileInstruction;
    if HayError then exit;   //aborta
    //se espera delimitador
    if cIn.Eof then break;  //sale por fin de archivo
    //busca delimitador
    ProcComments;
    if HayError then exit;   //puede dar error por código assembler o directivas
    if cIn.tokType=tkExpDelim then begin //encontró delimitador de expresión
      cIn.Next;   //lo toma
      ProcComments;  //quita espacios
      if HayError then exit;   //puede dar error por código assembler o directivas
    end else begin  //hay otra cosa
      GenError('";" expected.');  //por ahora
      exit;  //debe ser un error
    end;
  end;
end;
procedure TCompiler.CompilarArc(iniMem: word);
{Compila un programa en el contexto actual. Empieza a codificar el código a partir de
la posición iniMem}
  function StartOfSection: boolean;
  begin
    Result := (cIn.tokL ='var') or (cIn.tokL ='const') or
              (cIn.tokL ='type') or (cIn.tokL ='procedure');
  end;

var
  i: Integer;
begin
  ClearVars;       //limpia las variables
  ClearFuncs;      //limpia las funciones
  ClearAllConst;   //limpia las comstantes
  //ClearTypes;      //limpia los tipos
  //limpia el estado de las funciones del sistena
  for i:=0 to nIntFun-1 do begin
    funcs[i].adrr:=-1;  //para indicar que no están codificadas
  end;
  //Inicia PIC
  pic.ClearMemRAM;
  pic.ClearMemFlash;
  pic.iFlash:=0;       //posición a escribir
  _GOTO(iniMem);       //instrucción de salto inicial
  pic.iFlash:=iniMem;  //inicia puntero a Flash
  //inicia punteros para subrutinas
  iniBloSub := 1;       //empieza después del GOTO
  curBloSub := iniBloSub; //inicialmente está libre

  ExprLevel := 0;  //inicia
  Perr.Clear;
  ProcComments;
  if Perr.HayError then exit;
  if cIn.tokL = 'program' then begin
    cIn.Next;  //pasa al nombre
    ProcComments;
    if HayError then exit;   //puede dar error por código assembler o directivas
    if cIn.Eof then begin
      GenError('Name of program expected.');
      exit;
    end;
    cIn.Next;  //Toma el nombre y pasa al siguiente
    if not CaptureDelExpres then exit;
  end;
  if cIn.Eof then begin
    GenError('Expected "begin", "var", "type" or "const".');
    exit;
  end;
  ProcComments;
  if Perr.HayError then exit;
  //Empiezan las declaraciones
  while StartOfSection do begin
    if cIn.tokL = 'var' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'begin') do begin
        CompileVarDeclar;
        if pErr.HayError then exit;;
      end;
    end else if cIn.tokL = 'const' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'begin') do begin
        CompileConstDeclar;
        if pErr.HayError then exit;;
      end;
    end else if cIn.tokL = 'procedure' then begin
      cIn.Next;    //lo toma
      CompileProcDeclar;
    end else begin
      GenError('Not implemented: "%s"', [cIn.tok]);
      exit;
    end;
  end;
  {Verifica el traslape al final para darle tiempo a compilar todos los procedimientos
  y así tener una idea de hasta cuánta memoria se requerirá}
  if traslape or pErr.HayError then exit;
  //procesa cuerpo
  if cIn.tokL = 'begin' then begin
    Cod_StartProgram;
    cIn.Next;   //coge "begin"
    //codifica el contenido
    CompileCurBlock;   //compila el cuerpo
    if Perr.HayError then exit;
    if cIn.Eof then begin
      GenError('Unexpected end of file. "end" expected.');
      exit;       //sale
    end;
    if cIn.tokL <> 'end' then begin  //verifica si termina el programa
      GenError('"end" expected.');
      exit;       //sale
    end;
    cIn.Next;   //coge "end"
    //debería seguir el punto
    if cIn.tok <> '.' then begin
      GenError('"." expected.');
      exit;       //sale
    end;
    cIn.Next;
    //no debe haber más instrucciones
    cIn.SkipWhites;
    if not cIn.Eof then begin
      GenError('Syntax error. Nothing should be after "END."');
      exit;       //sale
    end;
    _SLEEP();   //agrega instrucción final
  end else begin
    GenError('Expected "begin", "var", "type" or "const".');
    exit;
  end;
  Cod_EndProgram;
end;
procedure TCompiler.Compilar(NombArc: string; LinArc: Tstrings);
//Compila el contenido de un archivo w ensamblador
var
  iniCOD: Integer;
  posCxt: TPosCont;
  nPass : integer;
begin
  //se pone en un "try" para capturar errores y para tener un punto salida de salida
  //único
  if ejecProg then begin
    GenError('There is a compilation in progress.');
    exit;  //sale directamente
  end;
  try
    ejecProg := true;  //marca bandera
    Perr.IniError;
    //Genera instrucciones de inicio
    cIn.ClearAll;       //elimina todos los Contextos de entrada
    //compila el texto indicado
    cIn.NewContextFromFile(NombArc, LinArc);   //Crea nuevo contenido
    posCxt := cIn.PosAct;    //Guarda la posición inicial del archivo
    if PErr.HayError then exit;
    {EL método de codificación consiste en dejar espacios fijos al inicio para ir
    llenándolos con las rutinas que sean usadas, e ir expandiendo este espacio en bloques
    mientras se vayan llenando.}
    iniCOD := 0;         //dirección de inicio del código principal
    nPass := 1;          //cuenta el número de pasadas
    repeat
      inc(iniCOD, $8);    //incrementa dirección de inicio
      cIn.PosAct := posCxt; //Posiciona al inicio
      finBloSub := iniCOD;  //_Límite de espacio, para posibles rutinas usadas
      debugln('*** Compilación ' + IntToStr(nPass) + ' en '+ IntToHex(iniCOD,3));
      Traslape := false;   //inicia bandera
      CompilarArc(iniCOD); //puede dar error
      if pErr.HayError then break;
      inc(nPass);
    until not traslape;
    if PErr.HayError then exit;
    //Se pudo compilar sin traslape y sin error. Se procede a juntar el código de subrutinas
    //con del programa principal, en una última compilación.
    iniCOD := curBloSub; //empieza a codificar exactamente en donde terminarán las subrutinas
    cIn.PosAct := posCxt; //Posiciona al inicio
    debugln('*** Compilación ' + IntToStr(nPass) + ' en '+ IntToHex(iniCOD,3));
    CompilarArc(iniCOD);
    if PErr.HayError then exit;  //No debería generar ya error

    cIn.QuitaContexEnt;   //es necesario por dejar limpio
    //genera archivo hexa
    pic.GenHex(rutApp + 'salida.hex');
  finally
    ejecProg := false;
    //tareas de finalización
    //como actualizar estado
  end;
end;

function TCompiler.RAMusage: string;
{Devuelve una cadena con información sobre el uso de la memoria.}
var
  dir: String;
  tmp: String;
  i: Integer;
begin
  tmp := '';
  for i:= 0 to high(vars) do begin
    dir := 'bnk'+ IntToStr(vars[i].bank) + ':$' + IntToHex(vars[i].offs, 3);
    if vars[i].typ = tipBool then begin
      tmp += ' ' + vars[i].nom + ' Db ' +  dir + LineEnding;
    end else if vars[i].typ = tipByte then begin
      tmp += ' ' + vars[i].nom + ' DB ' +  dir + LineEnding;
    end else if vars[i].typ = tipWord then begin
      tmp += ' ' + vars[i].nom + ' DW ' +  dir + LineEnding;
    end else begin
      tmp += ' "' + vars[i].nom + '"->' +  dir + LineEnding;
    end;
  end;
  Result := tmp;
end;
procedure TCompiler.DumpCode(l: TSTrings);
begin
//  AsmList := TStringList.Create;  //crea lista para almacenar ensamblador
  pic.DumpCode(l);
end;
procedure TCompiler.DumpStatistics(l: TSTrings);
var
  tot: Word;
  used: Word;
begin
  tot := pic.TotalMemRAM;
  if tot=0 then exit;  //protección
  used := pic.UsedMemRAM;
  l.Add('RAM Used   = ' + IntToStr(used) +'/'+ IntToStr(tot) + 'B (' +
        FloatToStrF(100*used/tot, ffGeneral, 1, 3) + '%)' );
  tot := pic.TotalMemFlash;
  used := pic.UsedMemFlash;
  l.Add('Flash Used = ' + IntToStr(used) +'/'+ IntToStr(tot) + ' (' +
        FloatToStrF(100*used/tot, ffGeneral, 1, 3) + '%)' );
end;

procedure TCompiler.DefLexDir;
{Define la sinatxis del lexer que se usará para analizar las directivas. La que
 debe estar enter lso símbolo {$ ... }
}
begin
  //solo se requiere identificadores y números
  lexDir.DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');
  lexDir.DefTokContent('[0-9]', '[0-9.]*', lexDir.tkNumber);
  lexDir.Rebuild;
end;
constructor TCompiler.Create;
begin
  inherited Create;
  pic := TPIC16.Create;
  lexDir := TSynFacilSyn.Create(nil);  //crea lexer para analzar directivas
  DefLexDir;
  cIn.OnNewLine:=@cInNewLine;
  ///////////define la sintaxis del compilador
  //crea y guarda referencia w los atributos
  tkEol      := xLex.tkEol;
  tkIdentif  := xLex.tkIdentif;
  tkKeyword  := xLex.tkKeyword;
  tkNumber   := xLex.tkNumber;
  tkString   := xLex.tkString;
  //personalizados
  tkOperator := xLex.NewTokType('Operador'); //personalizado
  tkBoolean  := xLex.NewTokType('Boolean');  //personalizado
  tkSysFunct := xLex.NewTokType('SysFunct'); //funciones del sistema
  tkExpDelim := xLex.NewTokType('ExpDelim');//delimitador de expresión ";"
  tkBlkDelim := xLex.NewTokType('BlkDelim'); //delimitador de bloque
  tkType     := xLex.NewTokType('Types');    //personalizado
  tkStruct   := xLex.NewTokType('Struct');   //personalizado
  tkDirective:= xLex.NewTokType('Directive'); //personalizado
  tkAsm      := xLex.NewTokType('Asm');      //personalizado
  tkOthers   := xLex.NewTokType('Others');   //personalizado
  //Configura atributos
  tkKeyword.Style := [fsBold];     //en negrita
  tkBlkDelim.Foreground:=clGreen;
  tkBlkDelim.Style := [fsBold];     //en negrita
  tkStruct.Foreground:=clGreen;
  tkStruct.Style := [fsBold];     //en negrita
  //inicia la configuración
  xLex.ClearMethodTables;           //limpìw tabla de métodos
  xLex.ClearSpecials;               //para empezar w definir tokens
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
  xLex.AddIdentSpecList('byte word boolean', tkType);
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

  StartSyntax;   //Debe hacerse solo una vez al inicio
  InitAsm(pic, self);   //inicia el procesamiento de ASM
end;
destructor TCompiler.Destroy;
begin
  lexDir.Destroy;
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
    dicSet('Not implemented.', 'No implementado');
    dicSet('Not implemented: "%s"', 'No implementado: "%s"');
    dicSet('Identifier expected.', 'Identificador esperado.');
    dicSet('Duplicated identifier: "%s"', 'Identificador duplicado: "%s"');
    dicSet('Unvalid float number.', 'Número decimal no válido.');
    dicSet('Very large number. Cannot process.', 'Número muy grande. No se puede procesar. ');
    dicSet('No type defined to accommodate this number.', 'No hay tipo definido para albergar a este número.');
    dicSet('Size of data not supported.', 'Tamaño de dato no soportado.');
    dicSet('RAM memory is full.', 'Memoria RAM agotada.');
    dicSet('Undefined type "%s"', 'Tipo "%s" no definido.');
    dicSet('"." expected.', 'Se esperaba "."');
    dicSet('Invalid memory address.', 'Dirección de memoria inválida.');
    dicSet('Invalid memory address for this device.', 'No existe esta dirección de memoria en este dispositivo.');
    dicSet('Identifier of variable expected.', 'Se esperaba identificador de variable.');
    dicSet('Identifier of constant expected.', 'Se esperaba identificador de constante');
    dicSet('Numeric address expected.', 'Se esperaba dirección numérica.');
    dicSet('Identifier of type expected.', 'Se esperaba identificador de tipo.');
    dicSet('":" or "," expected.', 'Se esperaba ":" o ",".');
    dicSet('"=", ":" or "," expected.', 'Se esperaba "=", ":" o ",".');
    dicSet('"begin" expected.', 'Se esperaba "begin".');
    dicSet('"end" expected.', 'Se esperaba "end".');
    dicSet('"." expected.', 'Se esperaba "."');
    dicSet('"(" expected.', 'Se esperaba "("');
    dicSet('":" expected.', 'Se esperaba ":"');
    dicSet('";" expected.', 'Se esperaba ";"');
    dicSet('Boolean expression expected.', 'Se esperaba expresión booleana.');
    dicSet('"do" expected.', 'Se esperaba "do".');
    dicSet('"then" expected.', 'Se esperaba "then"');
    dicSet('"until" expected.', 'Se esperaba "until"');
    dicSet('Unknown structure.', 'Estructura desconocida.');
    dicSet('Name of program expected.', 'Se esperaba nombre de programa.');
    dicSet('Expected "begin", "var", "type" or "const".', 'Se esperaba "begin", "var", "type" o "const".');
    dicSet('Unexpected end of file. "end" expected.', 'Inesperado fin de archivo. Se esperaba "end".');
    dicSet('There is a compilation in progress.', 'Ya se está compilando un programa actualmente.');
    dicSet('Constant expression expected.', 'Se esperaba una expresión constante');
    dicSet('Clock frequency not supported.', 'Frecuencia de reloj no soportada.');
    dicSet('Error in directive.', 'Error en directiva');
    dicSet('Unknown directive: %s', 'Directiva desconocida: %s');
    dicSet('Cannot increase a constant.', 'No se puede incrementar una constante.');
    dicSet('Cannot increase an expression.','No se puede incrementar una expresión.');
    dicSet('Cannot decrease a constant.', 'No se puede disminuir una constante.');
    dicSet('Cannot decrease an expression.','No se puede disminuir una expresión.');
    dicSet('Unknown device: %s', 'Dispositivo desconocido: %s');
    dicSet('Syntax error. Nothing should be after "END."', 'Error de sintaxis. Nada debe aparecer después de "END."');
  end;
  end;
end;

initialization
  //Es necesario crear solo una instancia del compilador.
  cxp := TCompiler.Create;  //Crea una instancia del compilador

finalization
  cxp.Destroy;
end.

