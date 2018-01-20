{Unidad que implementa a la clase TParserDirec, que sirve como contenedor para
implementar las funcionaliddes de procesamiento de directivas.
}
unit ParserDirec;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, ParserAsm, fgl, math, SynFacilHighlighter, Globales,
  XpresElementsPIC, Pic16Devices, MisUtils, XpresBas, LCLProc, Graphics;
type  //Tipos para manejo de expresiones
  TDirDatType = (ddtNumber, ddtString);

  { TDirOperand }
  //Tipo expresión u operando. Se usa para manejo de evaluación aritmética.
  TDirOperand = object
  private
    FvalStr: string;
    FvalNum: Double;
    function GetvalNum: Double;
    procedure SetvalNum(AValue: Double);
    function GetvalStr: string;
    procedure SetvalStr(AValue: string);
  public  //Campos principales
    datTyp: TDirDatType;  //Tipo de dato
    property valNum: Double read GetvalNum write SetvalNum ;    //Valor numérico de la expresión
    property valStr: string read GetvalStr write SetvalStr;  //Valor cadena de la expresión
    procedure SetBool(value: boolean);
  End;

  TDirEveReadNum =  function: Single of object;
  TDirEveWriteNum =  procedure(AValue: Single) of object;
  TDirEveReadStr =  function: String of object;
  TDirEveWriteStr =  procedure(AValue: String) of object;
  { TDirVar }
  //Define a una variable.
  TDirVar= class
  private  //Eventos
    {Estos eventos se usan cuando se requiere direccionar, la lectura/escritura de
    valores de la expresión.}
    OnReadNum: TDirEveReadNum;
    OnWriteNum: TDirEveWriteNum;
    OnReadStr: TDirEveReadStr;
    OnWriteStr: TDirEveWriteStr;
  private
    Fvalor: TDirOperand;
    function Getvalor: TDirOperand;
    procedure Setvalor(AValue: TDirOperand);
  public
    nomb: string;   //Nombre de la variable
    property valor: TDirOperand read Getvalor write Setvalor;
    procedure ReflectToNumber(ReadNum: TDirEveReadNum; WriteNum: TDirEveWriteNum);
    procedure ReflectToString(ReadStr: TDirEveReadStr; WriteStr: TDirEveWriteStr);
    //property datTyp: TDirDatType read Fvalor.datTyp write Fvalor.datTyp;
  end;
  TDirVar_list = specialize TFPGObjectList<TDirVar>;

type
  //Identifica a una macro
  TDirMacro = class
    name  : string;
    value : string;
    posDef: TSrcPos;   //posición del contexto
  end;

  TDirMacro_list = specialize TFPGObjectList<TDirMacro>;

  { TParserDirec }
  TParserDirec = class(TParserAsm)
  private  //Rutinas del evaluador de expresiones
    varsList : TDirVar_list;
    function CogCarERR(car: char): Boolean;
    function CogExpresionPar: TDirOperand;
    function CogIdentif(var s: string): boolean;
    function CogNumero(var n: Double): boolean;
    function CogOperando: TDirOperand;
    function cogOperador: String;
    function DefinedVar(cad: string; out dvar: TDirVar): boolean;
    procedure ProcINFO;
    procedure ProcMSGERR;
    procedure ProcMSGWAR;
    procedure ProcSET_MAPPED_RAM;
    procedure ProcMAP_RAM_TO_PIN;
    procedure ProcSET_UNIMP_BITS;
    procedure ProcCLEAR_STATE_RAM;
    procedure ProcSET_STATE_RAM;
    function read_CURRBANK: Single;
    function read_CURRBLOCK: String;
    function read_PIC_FREQUEN: Single;
    function read_PIC_IFLASH: Single;
    function read_PIC_MAXFLASH: Single;
    function read_PIC_MAXFREQ: Single;
    function read_PIC_MODEL: string;
    function read_PIC_NPINS: Single;
    function read_PIC_NUMBANKS: Single;
    function read_PIC_NUMPAGES: Single;
    function read_SYN_MODE: String;
    procedure write_CURRBANK(AValue: Single);
    procedure write_PIC_FREQUEN(AValue: Single);
    procedure write_PIC_IFLASH(AValue: Single);
    procedure write_PIC_MAXFLASH(AValue: Single);
    procedure write_PIC_MAXFREQ(AValue: Single);
    procedure write_PIC_MODEL(AValue: string);
    function Evaluar(Op1: TDirOperand; opr: String; Op2: TDirOperand): TDirOperand;
    function jerOp(operad: String): Integer;
    function CogExpresion(jerar: Integer): TDirOperand;
    function AsigVariable(VarName: string; const value: TDirOperand): TDirVar;
    procedure write_PIC_NPINS(AValue: Single);
    procedure write_PIC_NUMBANKS(AValue: Single);
    procedure write_PIC_NUMPAGES(AValue: Single);
    procedure write_SYN_MODE(AValue: String);
  private
    lexDir : TSynFacilSyn;  //lexer para analizar directivas
    tokIni : integer;  //Posición inicial del token actual
    dirOperator: Integer;
    dirDelimiter: integer;
    WaitForEndIF: integer;
    macroList : TDirMacro_list;
    procedure GenErrorDir(msg: string; const Args: array of const);
    procedure GenErrorDir(msg: string);
    procedure IniExplorDirec(out lin: string);
    function modeStr: string;
    procedure ProcCONFIG;
    procedure ProcFREQUENCY;
    procedure ProcDEFINE(lin: string);
    procedure ProcIFDEF(lin: string; negated: boolean);
    procedure ProcIF(lin: string; negated: boolean);
    procedure ProcINCLUDE(lin: string; var ctxChanged: boolean);
    procedure ProcOUTPUTHEX(lin: string);
    procedure ProcMODE;
    procedure ProcMSGBOX;
    procedure ProcWARNING;
    procedure ProcERROR;
    procedure ProcSET;
    procedure ProcPROCESSOR;
    function ScanIFDEF(out tok: string): boolean;
    procedure skipWhites;
    function GetIdent: string;
    function tokType: integer;
    procedure DefLexDirectiv;
  protected //Variables internas del compilador
    mainFile  : string;  //archivo inicial que se compila
    mode      : (modPascal, modPicPas);
    ConfigWord: integer;  //Bits de configuración
    procedure NewMacro(macName, macValue: string);
    procedure ProcDIRline(const AsmLin: string; out ctxChanged: boolean);
    function DefinedMacro(macName: string): boolean;
    function FindMacro(macName: string): TDirMacro;
  public
    hexFile  : string;  //Nombre de archivo de salida
    Compiling: boolean;  //Bandera para el compilado
    function ExpandRelPathTo(BaseFile, FileName: string): string;
    procedure ClearMacros;
    constructor Create; override;
    destructor Destroy; override;
  end;

  procedure SetLanguage;

implementation
{$I ..\language\tra_ParserDirec.pas}
{ TDirOperand }
function TDirOperand.GetvalNum: Double;
begin
  if datTyp = ddtNumber then begin
    Result := FvalNum;
  end else begin  //es cadena
    //Trata de obtener su valor numérico
    if not TryStrToFloat(FvalStr , Result) then exit(0);
  end;
end;
procedure TDirOperand.SetvalNum(AValue: Double);
begin
  datTyp := ddtNumber;   //fuerza a que sea numéro
  FvalNum := AValue;
end;
function TDirOperand.GetvalStr: string;
begin
  if datTyp = ddtString then begin
    Result := FvalStr;
  end else begin  //es número
    Result := FloatToStr(FvalNum);
  end;
end;
procedure TDirOperand.SetvalStr(AValue: string);
begin
  datTyp := ddtString;   //fuerza a que sea string
  FvalStr := AValue;
end;
procedure TDirOperand.SetBool(value: boolean);
{Asigna un valro booleano al operando. En realidad, no hay valores booleando, así
que se usará los números 0 o 1}
begin
  if value then valNum := 1 else valNum := 0;
end;
{ TDirVar }
function TDirVar.Getvalor: TDirOperand;
begin
  //Primero actualiza en caso de que este enlazada.
  {Se actualizan ambos tipos porque, si por ejemplo, el tipo es numérico, pero se pide
  como cadena, se debe tener actualizdo su valor numérico, por si se hace una conversión.
  Habría que ver, si esta es la forma más óptima, de implementarlo.}
  if OnReadNum<>nil then Fvalor.valNum := OnReadNum();
  if OnReadStr<>nil then Fvalor.valStr := OnReadStr();
  //Ahora devuelve valor actualizado
  Result := Fvalor;
end;
procedure TDirVar.Setvalor(AValue: TDirOperand);
begin
  Fvalor := AValue;  //actualiza
  //Llama a eventos para actualziar valor reflejado
  if OnWriteNum<>nil then OnWriteNum(Fvalor.FvalNum);
  if OnWriteStr<>nil then OnWriteStr(Fvalor.FvalStr);
end;
procedure TDirVar.ReflectToNumber(ReadNum: TDirEveReadNum;
  WriteNum: TDirEveWriteNum);
{Define que la variable esté reflejada a otra variable numérica, mediante eventos.}
begin
  Fvalor.datTyp := ddtNumber;  //fija como número
  OnReadNum := ReadNum;        //asigna eventos
  OnWriteNum := WriteNum;
end;
procedure TDirVar.ReflectToString(ReadStr: TDirEveReadStr;
  WriteStr: TDirEveWriteStr);
begin
  Fvalor.datTyp := ddtString;  //fija como cadena
  OnReadStr := ReadStr;        //asigna eventos
  OnWriteStr := WriteStr;
end;
{ TParserDirec }
procedure TParserDirec.GenErrorDir(msg: string);
{Genera un error corrigiendo la posición horizontal}
var
  p: TSrcPos;
begin
  p := cIn.ReadSrcPos;
  p.col := tokIni + lexDir.GetX;  //corrige columna
  GenErrorPos(msg, [], p);
end;
procedure TParserDirec.GenErrorDir(msg: string; const Args: array of const);
var
  p: TSrcPos;
begin
  p := cIn.ReadSrcPos;
  p.col := tokIni + lexDir.GetX;  //corrige columna
  GenErrorPos(msg, Args, p);
end;
function TParserDirec.modeStr: string;
begin
  case mode of
  modPascal: Result := 'modPascal';
  modPicPas: Result := 'modPicPas';
  else
    Result := 'Unknown';
  end;
end;
function TParserDirec.tokType: integer;
begin
  Result := lexdir.GetTokenKind;
end;
procedure TParserDirec.skipWhites;
begin
  if tokType = lexDir.tnSpace then
    lexDir.Next;  //quita espacios
end;
function TParserDirec.CogCarERR(car: char): Boolean;
{Coge el caracter indicado. Si no lo encuentra genera error y devuelve FALSE.}
begin
  if lexDir.GetToken=car then begin
    //Es el caracter buscado
    lexDir.Next;
    exit(true);
  end else begin
    GenErrorDir(ER_EXPECT_CAR_, [car]);
    exit(false);
  end;
end;
function TParserDirec.GetIdent: string;
begin
  if tokType = lexDir.tnSpace then
    lexDir.Next;  //quita espacios
  //verifica
  if lexDir.GetTokenKind <> lexDir.tnIdentif then begin
    GenErrorDir(ER_IDENT_EXPEC);
    exit;
  end;
  Result := lexDir.GetToken;
  lexDir.Next;  //toma identificador
end;
function TParserDirec.DefinedVar(cad: string; out dvar: TDirVar): boolean;
{Indica si un identificador corresponde a una variable. Devuelve la referencia a la
variable encontrada.}
begin
  cad := UpCase(cad);
  for dvar in varsList do begin
    if UpCase(dvar.nomb) = cad then begin
      exit(true);
    end;
  end;
  exit(false);
end;
function TParserDirec.CogExpresionPar: TDirOperand;
{Coge una expresión que debe estar encerrada entre paréntesis. Puede genera error}
begin
  if not CogCarERR('(') then exit;  //sale con error
  Result := CogExpresion(0);
  if HayError then exit;  //sale con error
  skipWhites;
  if not CogCarERR(')') then exit;  //sale con error
end;
function TParserDirec.CogNumero(var n: Double): boolean;
{Veririfca si lo que sigues es un número y de ser así, intenta tomarlo.
Puede generar error al convertir el número}
var
  m: Longint;
begin
  if lexdir.GetTokenKind <> lexdir.tnNumber then exit(false);
  if lexdir.GetToken[1] = '$' then begin
    //Formato hexadecimal
    if not TryStrToInt(lexdir.GetToken, m) then begin
      GenErrorDir(ER_ERIN_NUMBER_, [lexDir.GetToken]);
      exit(false);
    end;
    n := m;
  end else begin
    if not TryStrToFloat(lexdir.GetToken, n) then begin
      GenErrorDir(ER_ERIN_NUMBER_, [lexDir.GetToken]);
      exit(false);
    end;
  end;
  lexdir.Next;
  Result := true;  //indica que hubo número
end;
function TParserDirec.CogIdentif(var s: string): boolean;
{Veririfca si lo que sigues es un identificador y de ser así, intenta tomarlo.}
begin
  if tokType = lexDir.tnSpace then
    lexDir.Next;  //quita espacios
  //verifica
  if lexDir.GetTokenKind <> lexDir.tnIdentif then begin
    exit(false);
  end;
  s := lexDir.GetToken;
  lexDir.Next;  //toma identificador
  exit(true);
end;
function TParserDirec.CogOperando: TDirOperand;
{Coge un operando en la posición actual del contexto. Si no enceuntra
el operando o es erróneo, genera Error.}
var
  cad , tmp: String;
  num : Double;
  exp : TDirOperand;
  mac: TDirMacro;
  p: TFaLexerState;
  dvar: TDirVar;
  delim: Char;
begin
  skipWhites;   //quita blancos iniciales
  if lexDir.GetEol then begin
    Result.datTyp := ddtString;
    Result.FvalStr := '';
    exit;
  end;
  if CogNumero(num) then begin
    if HayError then exit;  //pudo haber error en número
    Result.valNum := num;   //fija tipo a número
  end else if lexDir.GetTokenKind = lexDir.tnString then begin
    //Es cadena
    tmp := lexDir.GetToken;
    delim := tmp[1];
    tmp := copy(tmp, 2, length(tmp)-2);  //quita delimitadores
    if delim='"' then begin
      //Es cadena con comilla doble
      tmp := StringReplace(tmp, '\n', LineEnding, [rfReplaceAll]);
      tmp := StringReplace(tmp, '\r', chr($0D), [rfReplaceAll]);
      tmp := StringReplace(tmp, '\t', #9, [rfReplaceAll]);
    end;
    Result.valStr := tmp;
    lexDir.Next;
  end else if CogIdentif(cad) then begin
    {Es un identificador}
    //Busca si es macro
    mac := FindMacro(cad);
    if mac<>nil then begin
      //Es una macro. Hay que expandirla
      p := lexDir.State;  //guarda estado de lexer
      lexDir.SetLine(mac.value, 0);  //inicia otra explroación en el contenido de la macro
      Result := CogExpresion(0);
      lexDir.State := p;  //restaura estado del lexer, para que siga la expresión
      exit;
    end;
    //Busca si es una variable
    cad := UpCase(cad);
    for dvar in varsList do begin
      if UpCase(dvar.nomb) = cad then begin
        Result := dvar.valor;
        exit;
      end;
    end;
    //No es variable, ni macro, busca si es función
    case cad of
    'ABS': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valNum := abs(exp.valNum);
      exit;  //sale sin error
    end;
    'SGN': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valNum := Sign(exp.valNum);
      exit;  //sale sin error
    end;
    'SIN': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valNum := sin(exp.valNum);
      exit;  //sale sin error
    end;
    'COS': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valNum := cos(exp.valNum);
      exit;  //sale sin error
    end;
    'TAN': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valNum := tan(exp.valNum);
      exit;  //sale sin error
    end;
    'LOG': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valNum := ln(exp.valNum);
      exit;  //sale sin error
    end;
    'ROUND': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valNum := round(exp.valNum);
      exit;  //sale sin error
    end;
    'TRUNC': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valNum := trunc(exp.valNum);
      exit;  //sale sin error
    end;
    'LENGTH': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valNum := length(exp.valStr);
      exit;  //sale sin error
    end;
    'UPCASE': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valStr := upcase(exp.valStr);
      exit;  //sale sin error
    end;
    'LOWCASE': begin
      exp := CogExpresionPar;
      if HayError then exit;
      Result.valStr := LowerCase(exp.valStr);
      exit;  //sale sin error
    end;
    end;
    //No es variable ni función.
    GenErrorDir(ER_UNKNW_IDENT_, [cad]);
  end else If lexDir.GetToken = '(' Then begin
    Result := CogExpresionPar;
    exit;  //Puede salir con error
  end else If lexDir.GetToken = '-' Then begin
    //Puede ser número negativo
    lexDir.Next;  //toma el signo
    Result := CogOperando();
    if HayError then exit;
    if Result.datTyp <> ddtNumber then begin
      GenErrorDir(ER_SYNTAX_ERRO);
      exit;  //no devuelve nada
    end;
    //Es un número
    Result.valNum := -Result.valNum;
    exit;  //Puede salir con error
  end else begin
    //Debe ser otra cosa
    GenErrorDir(ER_SYNTAX_ERRO);
    exit;  //no devuelve nada
  end;
end;
function TParserDirec.cogOperador: String;
{Coge un operador en la posición del contexto actual. Si no encuentra
 devuelve cadena vacía y no coge caracteres, salvo espacios iniciales.}
begin
  Result := '';
  skipWhites;     //quita blancos iniciales
  Case UpCase(lexDir.GetToken) of //completa con operador de más caracteres
  '+': begin
         Result := lexDir.GetToken;
         lexDir.next;
        end;
  '-': begin
         Result := lexDir.GetToken;
         lexDir.next;
      end;
  '*': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  '/': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  '\': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  '%': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  '^': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  //Operadores de comparación
  '=': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  '<>': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  '>': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  '<': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  '>=': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  '<=': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  'AND': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  'OR': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  'XOR': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  'NOT': begin
        Result := lexDir.GetToken;
        lexDir.next;
      end;
  end;
end;
function TParserDirec.jerOp(operad: String): Integer;
//Devuelve la jerarquía de un operador ver documentación técnica.
begin
    case operad of
    'OR','XOR'             : Result := 2;
    'AND'                  : Result := 3;
    '=', '<>', '>', '<', '>=', '<=': Result := 4;
    '+', '-'               : Result := 5;
    '*', '/', '\', '%'     : Result := 6;
    '^', 'NOT'             : Result := 8;
    else
      Result := 0;
    end;
End;
function TParserDirec.Evaluar(Op1: TDirOperand; opr: String; Op2: TDirOperand): TDirOperand;
//Devuelve el resultado y tipo de una operación
begin
    Case opr of
    '': begin     //Sin operador. Y se supone sin Op2
          //no hay nada que hacer, ya está en la pila
          Result := Op1;
        end;
    '+': begin
           //Puede ser concatenación o suma
           if (Op1.datTyp = ddtString) or (Op2.datTyp = ddtString) then begin
             //Al menos uno es cadena
             Result.valStr := Op1.valStr + Op2.valStr;  //cadena
           end else begin
             //Son dos números, los suma
             Result.valNum := Op1.valNum + Op2.valNum;  //número
           end;
         end;
    '-': begin
          Result.valNum := Op1.valNum - Op2.valNum;
         end;
    '*': begin
          Result.valNum := Op1.valNum * Op2.valNum;
         end;
    '/': begin
          if Op2.valNum = 0 Then
              GenErrorDir(ER_DIVIDE_ZERO)
          else begin   //error
              Result.valNum := Op1.valNum / Op2.valNum;
          End;
         end;
    '\': begin
          if Op2.valNum = 0 then
              GenErrorDir(ER_DIVIDE_ZERO)
          else begin   //error
              Result.valNum := round(Op1.valNum) div round(Op2.valNum);
          end;
         end;
    '%': begin
          if Op2.valNum = 0 then
              GenErrorDir(ER_DIVIDE_ZERO)
          else begin   //error
              Result.valNum := round(Op1.valNum) mod round(Op2.valNum);
          end;
         end;
    '^': begin
          if (Op2.valNum = 0) and (Op2.valNum = 0) then
              GenErrorDir(ER_EVA_ZER_ZER)
          else begin   //error
              Result.valNum := power(Op1.valNum, Op2.valNum);
          end;
         end;
    //Operadores de comparación
    '=': begin
          //Puede ser concatenación o suma
          if (Op1.datTyp = ddtString) or (Op2.datTyp = ddtString) then begin
            //Al menos uno es cadena, compara cadenas
            Result.SetBool(Op1.valStr = Op2.valStr);
          end else begin
            //Son dos números, compara valores
            Result.SetBool(Op1.valNum = Op2.valNum);
          end;
         end;
    '<>': begin
          //Puede ser concatenación o suma
          if (Op1.datTyp = ddtString) or (Op2.datTyp = ddtString) then begin
            //Al menos uno es cadena, compara cadenas
            Result.SetBool(Op1.valStr <> Op2.valStr);
          end else begin
            //Son dos números, compara valores
            Result.SetBool(Op1.valNum <> Op2.valNum);
          end;
         end;
    '>': begin
          //Puede ser concatenación o suma
          if (Op1.datTyp = ddtString) or (Op2.datTyp = ddtString) then begin
            //Al menos uno es cadena, compara cadenas
            Result.SetBool(Op1.valStr > Op2.valStr);
          end else begin
            //Son dos números, compara valores
            Result.SetBool(Op1.valNum > Op2.valNum);
          end;
         end;
    '<': begin
          //Puede ser concatenación o suma
          if (Op1.datTyp = ddtString) or (Op2.datTyp = ddtString) then begin
            //Al menos uno es cadena, compara cadenas
            Result.SetBool(Op1.valStr < Op2.valStr);
          end else begin
            //Son dos números, compara valores
            Result.SetBool(Op1.valNum < Op2.valNum);
          end;
         end;
    '>=': begin
          //Puede ser concatenación o suma
          if (Op1.datTyp = ddtString) or (Op2.datTyp = ddtString) then begin
            //Al menos uno es cadena, compara cadenas
            Result.SetBool(Op1.valStr >= Op2.valStr);
          end else begin
            //Son dos números, compara valores
            Result.SetBool(Op1.valNum >= Op2.valNum);
          end;
         end;
    '<=': begin
          //Puede ser concatenación o suma
          if (Op1.datTyp = ddtString) or (Op2.datTyp = ddtString) then begin
            //Al menos uno es cadena, compara cadenas
            Result.SetBool(Op1.valStr <= Op2.valStr);
          end else begin
            //Son dos números, compara valores
            Result.SetBool(Op1.valNum <= Op2.valNum);
          end;
         end;
    //Operadores lógicos
    //Falta ...
    else begin
        GenErrorDir(ER_OPE_NOT_IMP_, [opr]);
        Exit;
         End;
    end;
end;
function TParserDirec.CogExpresion(jerar: Integer): TDirOperand;
{ Evaluador de expresiones. Toma una expresión completa, en la posición actual del
contenido. Si no encuentra una expresión, genera error. }
var Op1, Op2 : TDirOperand;
    opr, opr2 : String;
    jerOpr, jerOpr2: Integer;
    pos1, pos2: TFaLexerState;
begin
    skipWhites;  //quita blancos iniciales
    Op1 := CogOperando;  //error
    if HayError then exit;
    opr := cogOperador;
    if opr = '' Then begin
      Result := Op1;
      Exit
    End;
    jerOpr := jerOp(opr);     //Hay operador, tomar su jerarquía
    //-------------------------- ¿Delimitada por jerarquía? ---------------------
    if jerOpr <= jerar then begin  //es menor que la que sigue, expres.
      Result := Op1;  //solo devuelve el único operando que leyó
      Exit;
    End;
    while opr <> '' do begin
        pos1 := lexDir.State;    //Guarda por si lo necesita
        Op2 := CogOperando;
        if HayError then exit;
        pos2 := lexDir.State;    //Guarda por si lo necesita
        opr2 := cogOperador;
        If opr2 <> '' Then begin  //Hay otro operador
            jerOpr2 := jerOp(opr2);
            //¿Delimitado por jerarquía de operador?
            If jerOpr2 <= jerar Then begin  //sigue uno de menor jerarquía, hay que salir
                lexDir.State:= pos2;   //antes de coger el operador
                Result := Evaluar(Op1, opr, Op2);
                Exit;
            End;
            If jerOpr2 > jerOpr Then begin    //y es de mayor jerarquía, retrocede
                lexDir.State:= pos1;        //retrocede
                Op2 := CogExpresion(jerOpr);        //evalua primero
                opr2 := cogOperador;    //actualiza el siguiente operador
            End;
        End;

        Op1 := Evaluar(Op1, opr, Op2);    //evalua resultado
        if HayError then exit;
        opr := opr2;
        jerOpr := jerOp(opr);    //actualiza operador anterior
    end;
    Result := Op1;
end;
function TParserDirec.AsigVariable(VarName: string; const value: TDirOperand): TDirVar;
{Asigna un valor numérico o de cadena a una variable. Si no existe la crea.
Devuelve la referencia a la variable asignada.}
begin
  //Busca variable
  if DefinedVar(VarName, Result) then begin
    //Encontró la variable
    Result.valor := value;
    exit(Result);
  end;
  //No se encontró, se debe crear
  Result := TDirVar.Create;
  Result.nomb := VarName;
  Result.valor := value;
  varsList.Add(Result);
end;
procedure TParserDirec.NewMacro(macName, macValue: string);
{Agrega una nueva macro a la lista de macros}
var
  mac: TDirMacro;
begin
  mac := TDirMacro.Create;
  mac.name := macName;
  mac.value := macValue;
  //Ubica la posición del contexto
  mac.posDef := cIn.ReadSrcPos;
  macroList.Add(mac);
end;
function TParserDirec.DefinedMacro(macName: string): boolean;
{Indica si una macro ha sido definida}
var
  mac: TDirMacro;
begin
  macName := UpCase(macName);
  for mac in macroList do begin
    if UpCase(mac.name) = macName then begin
      exit(true);  //encontró
    end;
  end;
//No se encontró
  exit(false);
end;
function TParserDirec.FindMacro(macName: string): TDirMacro;
{Busca la definición de una macro, si la encuentra, devuelve la referecnia, de otra
forma dewvuelve NIL.}
var
  mac: TDirMacro;
begin
  macName := UpCase(macName);
  for mac in macroList do begin
    if UpCase(mac.name) = macName then begin
      exit(mac);  //encontró
    end;
  end;
//No se encontró
  exit(nil);
end;
function TParserDirec.ExpandRelPathTo(BaseFile, FileName: string): string;
{Convierte una ruta relativa (FileName), a una absoluta, usnado como base la ruta de
otro archivo (BaseFile)}
var
  BasePath: RawByteString;
begin
   if pos(DirectorySeparator, FileName)=0 then begin
     //Ruta relativa. Se completa
     BasePath := ExtractFileDir(BaseFile);
     if BasePath = '' then begin
       //No hay de donde completar, usa la ruta actual
       Result := ExpandFileName(FileName);
     end else  begin
       Result := ExtractFileDir(BaseFile) + DirectorySeparator + FileName;
     end;
   end else begin
     //Tiene "DirectorySeparator", se asume que es ruta absoluta, y no se cambia.
     Result := FileName;
   end;
end;
function TParserDirec.ScanIFDEF(out tok: string): boolean;
{Explora el texto, hasta encontrar la directiva $ENDIF o $ELSE.  Si llega al
 final del contexto, sin encontrar alguna de estas directivas, devuelve FALSE.}
var
  tmp, direc: string;
begin
  while not cIn.Eof do begin
//    debugln(cIn.tok);
    if cIn.tokType = tnDirective then begin
      //Podría ser el delimitador buscado
      IniExplorDirec(tmp);
      direc := UpperCase(lexDir.GetToken);
      if (direc = 'ENDIF') or (direc='ELSE') then begin
        //Encontró el delimitador
        tok := direc;
        cIn.Next;  //toma el token
        exit(true);  //y continúa
      end;
    end;
    cIn.Next;
  end;
  //No encontró
  exit(false);
end;
procedure TParserDirec.ProcPROCESSOR;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  if not GetHardwareInfo(pic, lexDir.GetToken) then begin
    GenErrorDir(ER_UNKNO_DEVIC, [lexDir.GetToken]);
    exit;
  end;
end;
procedure TParserDirec.ProcFREQUENCY;
var
  f: Longint;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  if tokType <> lexDir.tnNumber then begin
    GenErrorDir(ER_ERROR_DIREC);
    exit;
  end;
  if not TryStrToInt(lexDir.GetToken, f) then begin
    GenErrorDir(ER_ERROR_FREQ);
    exit;
  end;
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  case UpperCase(lexDir.GetToken) of
  'KHZ': f := f * 1000;
  'MHZ': f := f * 1000000;
  else
    GenErrorDir(ER_ERROR_DIREC);
    exit;
  end;
  if f>pic.MaxFreq then begin
    GenErrorDir(ER_TOOHIGHFRE);
    exit;
  end;
  pic.frequen:=f; //asigna frecuencia
end;
procedure TParserDirec.ProcCONFIG;
var
  Ident, tmp: String;
  mac: TDirMacro;
  valBit: Longint;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  if lexDir.GetTokenKind = lexDir.tnNumber then begin
    //Es un valor numérico
    if not TryStrToInt(lexDir.GetToken, valBit) then begin
      GenErrorDir(ER_INVAL_CBIT_, [Ident]);
      exit;
    end;
    //Ya se tiene el valor numérico
    ConfigWord := valBit;  //carga directamente
    lexDir.Next;
    skipWhites;
    //No debe seguir nada
    if not lexDir.GetEol then begin
      GenErrorDir(ER_INVAL_CBIT_, [Ident]);
      exit;
    end;
    exit;
  end;
  //Debe seguir identificadores
  while not lexDir.GetEol do begin
    Ident := GetIdent;
    if HayError then exit;
    mac := FindMacro(Ident);
    if mac <> nil then begin
      //Hay macro definida. Hay que extraer su valor.
      tmp := trim(mac.value);
      if copy(tmp,1,2) = '0x' then begin
        delete(tmp,1,2);
        tmp := '$' + tmp;
      end;
      if not TryStrToInt(tmp, valBit) then begin
        GenErrorDir(ER_INVAL_CBIT_, [Ident]);
        exit;
      end;
      //Ya se tiene el valor numérico
      if ConfigWord = -1 then begin
        //Es la primera vez que se inicia
        ConfigWord := $FFFF;
      end;
      ConfigWord := ConfigWord and valBit;  //Marca bandera

      //msgbox('|'+mac.value+'|');
    end else begin
      //No está definido
      GenErrorDir(ER_CONF_UNDEF_, [Ident]);
      exit;
    end;
    skipWhites;
    if lexDir.GetToken = ',' then begin
      lexDir.Next;  //pasa al siguiente
    end;
    skipWhites;
  end;
end;
procedure TParserDirec.ProcINCLUDE(lin: string; var ctxChanged: boolean);
{Implementa la inclusión de un archivo externo en el código}
var
  filPath: string;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  //Toma el restante de la cadena
  filPath := copy(lin, lexDir.GetX);
  //Completa ruta, si es relativa
  if (pos('/', filPath)=0) and (pos('\', filPath)=0) then begin
    //No incluye información de ruta. Asume que está en la misma ruta.
    filPath := ExtractFileDir(mainFile) + DirectorySeparator + filPath;
  end;
  if not FileExists(filPath) then begin
    GenErrorDir(ER_FILE_NO_FND_, [filPath]);
    exit;
  end;
  //Ya se tiene el archivo
  cIn.Next;  //pasa la directiva
  cIn.NewContextFromFile(filPath);  //Pasa a explorar contenido del archivo
  cIn.curCon.autoClose := true;   //Para que se cierre, al finalizar
  //cIn.curCon.FixErrPos := true;   //Para que se ignore la posición de los errores
  //cIn.curCon.ErrPosition := p;    //Posición a usar para ubicar el error
  //cIn.curCon.PreErrorMsg := 'Macro '+mac.name+': ';
  ctxChanged := true;   //Marca bandera para indciar que se ha cambiado de contexto

end;
procedure TParserDirec.ProcOUTPUTHEX(lin: string);
var
  filPath: String;
begin
  lexDir.Next;  //pasa al siguiente
  filPath := CogExpresion(0).valStr;
  if HayError then Exit;
  filPath := ExpandRelPathTo(mainFile, filPath);  //Completa ruta, si es relativa
  //Por simplicidad se permite realizar esto en la primera y segunda pasada
  //Auqnue lo más práctico sería en la segunda pasada donde se genera el HEX final.
  hexfile := filPath;
end;
procedure TParserDirec.ProcDEFINE(lin: string);
var
  Ident, value: String;
begin
  lexDir.Next;  //pasa al siguiente
  Ident := GetIdent;
  if HayError then exit;
  skipWhites;
  if lexDir.GetEol then begin
    //Se definió un identificador sin valor
    NewMacro(Ident, '');
  end else begin
    //Sigue algo más
    if lexDir.GetToken <> '=' then begin
      GenErrorDir(ER_EXPEC_EQUAL);
      exit;
    end;
    lexDir.Next;  //toma símbolo
    skipWhites;
    if lexDir.GetEol then begin
      GenErrorDir(ER_SYNTAX_ERRO);
    end;
    //Toma definición
    value := copy(lin, lexDir.GetX, length(lin));
    NewMacro(Ident, value);
  end;
end;
procedure TParserDirec.ProcIFDEF(lin: string; negated: boolean);
  function EvaluateExp(const Ident: string): boolean;
  {Evalúa el resultado de la expresión de la directiva $IFDEF.
  Debería ejecutarse solo una vez, en ProcIFDEF(()}
  var
    xDirec: TxpEleDIREC;
    ele: TxpElement;
    dvar: TDirVar;
  begin
    if FirstPass then begin
      //Agrega el nodo para guardar información para la segunda pasada
      xDirec := TxpEleDIREC.Create;
      xDirec.srcDec := cIn.ReadSrcPos;   //guarda posición de aparición
      TreeDirec.AddElement(xDirec, false);  //Agrega sin verificación de nombre
      //Evalúa
      Result := (DefinedMacro(Ident) or DefinedVar(Ident, dvar)) xor negated;
      //Guarda resultado
      xDirec.ifDefResult := Result;
    end else begin
      {En al segunda pasada, ya no se evalúa, porque la segunda pasada, ya no se
      hace en el orden del código fuente, y se pierde la secuencia de directivas.}
      for ele in TreeDirec.curNode.elements do begin
        //Busca la directiva de la dirección actual (ubicada en la primera pasada)
        if ele.srcDec.EqualTo(cIn.ReadSrcPos) then begin
          //Encontró
          Result := TxpEleDIREC(ele).ifDefResult;
          exit;
        end;
      end;
      //No encontró. Esto no debería pasar.
      MsgErr('Implementation error.');
    end;
  end;
var
  Ident, direc: String;
begin
  lexDir.Next;  //pasa al siguiente
  Ident := GetIdent;
  if HayError then exit;
  skipWhites;
  if lexDir.GetEol then begin
    //Esto es lo normal. Buscamos el identificador
    if EvaluateExp(Ident) then begin
      //Está definido
      inc(WaitForEndIF);  //marca bandera para esperar
    end else begin
      //No está definido, no se debe compilar hasta un {$ENDIF} o un {$ELSE}
      cIn.Next;  //toma token {$IDEF  }
      //Explora, sin compilar, hasta encontrar directiva delimitadora.
      if not ScanIFDEF(direc) then begin
        //Llegó al final del código fuente, sin encontrar el ENDIF
        GenErrorDir(ER_ENDIF_NOFOU);
        exit;
      end;
      //Encontró token delimitador
      //Si es $ENDIF, no hay problema, todo termina allí, pero si es un else:
      if direc='ELSE' then begin
        inc(WaitForEndIF);  //marca bandera para esperar
      end;
    end;
  end else begin
    //Sigue algo más. No se esperaba.
    GenErrorDir(ER_SYNTAX_ERRO);
  end;
end;
procedure TParserDirec.ProcIF(lin: string; negated: boolean);
  function EvaluateExp(const Ident: string): boolean;
  {Evalúa el resultado de la expresión de la directiva $IFDEF.
  Debería ejecutarse solo una vez, en ProcIFDEF(()}
  var
    xDirec: TxpEleDIREC;
    ele: TxpElement;
    varValue: TDirOperand;
  begin
    if FirstPass then begin
      //Agrega el nodo para guardar información para la segunda pasada
      xDirec := TxpEleDIREC.Create;
      xDirec.srcDec := cIn.ReadSrcPos;   //guarda posición de aparición
      TreeDirec.AddElement(xDirec, false);  //Agrega sin verificación de nombre
      //Evalúa
      varValue := CogExpresion(0);
      //No debería seguir nada más
      if not lexDir.GetEol then begin
        GenErrorDir(ER_SYNTAX_ERRO);
        exit;
      end;
      if varValue.datTyp = ddtNumber then begin
        //En números, cualquier valor <>0 se considera verdadero
        Result := (varValue.valNum<>0) xor negated;
      end else begin
        //En cadenas, cualquier cadena no nula se considera verdadero
        Result := (varValue.valStr<>'') xor negated;
      end;
      //Guarda resultado
      xDirec.ifDefResult := Result;
    end else begin
      {En al segunda pasada, ya no se evalúa, porque la segunda pasada, ya no se
      hace en el orden del código fuente, y se pierde la secuencia de directivas.}
      for ele in TreeDirec.curNode.elements do begin
        //Busca la directiva de la dirección actual (ubicada en la primera pasada)
        if ele.srcDec.EqualTo(cIn.ReadSrcPos) then begin
          //Encontró
          Result := TxpEleDIREC(ele).ifDefResult;
          exit;
        end;
      end;
      //No encontró. Esto no debería pasar.
      MsgErr('Implementation error.');
    end;
  end;
var
  Ident, direc: String;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  if lexDir.GetEol then begin
    GenErrorDir(ER_SYNTAX_ERRO);
    exit;
  end;
  //Se supone que sigue una expresión
  if EvaluateExp(Ident) then begin
    //Es verdadero
    inc(WaitForEndIF);  //marca bandera para esperar
  end else begin
    //No es verdadero, no se debe compilar hasta un {$ENDIF} o un {$ELSE}
    cIn.Next;  //toma token {$IDEF  }
    //Explora, sin compilar, hasta encontrar directiva delimitadora.
    if not ScanIFDEF(direc) then begin
      //Llegó al final del código fuente, sin encontrar el ENDIF
      GenErrorDir(ER_ENDIF_NOFOU);
      exit;
    end;
    //Encontró token delimitador
    //Si es $ENDIF, no hay problema, todo termina allí, pero si es un else:
    if direc='ELSE' then begin
      inc(WaitForEndIF);  //marca bandera para esperar
    end;
  end;
end;
procedure TParserDirec.ProcMODE;
var
  txtMode: String;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  txtMode := UpCase(lexDir.GetToken);
  if txtMode = 'PICPAS' then begin
    self.mode := modPicPas;
  end else if txtMode = 'PASCAL' then begin
    self.mode := modPascal;
  end else begin
    GenErrorDir(ER_MODE_UNKNOWN, [txtMode]);
    exit;
  end;
end;
procedure TParserDirec.ProcMSGBOX;
var
  txtMsg: String;
begin
  lexDir.Next;  //pasa al siguiente
  txtMsg := CogExpresion(0).valStr;
  if HayError then Exit;
  //Solo muestra en compilación y en la primera pasada
  if Compiling and FirstPass then msgbox(txtMsg);
end;
procedure TParserDirec.ProcMSGERR;
var
  txtMsg: String;
begin
  lexDir.Next;  //pasa al siguiente
  txtMsg := CogExpresion(0).valStr;
  if HayError then Exit;
  //Solo muestra en compilación y en la primera pasada
  if Compiling and FirstPass then MsgErr(txtMsg);
end;
procedure TParserDirec.ProcMSGWAR;
var
  txtMsg: String;
begin
  lexDir.Next;  //pasa al siguiente
  txtMsg := CogExpresion(0).valStr;
  if HayError then Exit;
  //Solo muestra en compilación y en la primera pasada
  if Compiling and FirstPass then MsgExc(txtMsg);
end;
procedure TParserDirec.ProcINFO;
var
  txtMsg: String;
begin
  lexDir.Next;  //pasa al siguiente
  txtMsg := CogExpresion(0).valStr;
  if HayError then Exit;
  //Solo muestra en compilación y en la primera pasada
  if Compiling and FirstPass then GenInfo(txtMsg);
end;
procedure TParserDirec.ProcWARNING;
var
  txtMsg: String;
begin
  lexDir.Next;  //pasa al siguiente
  txtMsg := CogExpresion(0).valStr;
  if HayError then Exit;
  //Solo muestra en compilación y en la primera pasada
  if Compiling and FirstPass then GenWarn(txtMsg);
end;
procedure TParserDirec.ProcERROR;
var
  txtMsg: String;
begin
  lexDir.Next;  //pasa al siguiente
  txtMsg := CogExpresion(0).valStr;
  if HayError then Exit;
  //Solo muestra en compilación y en la primera pasada
  if Compiling and FirstPass then GenError(txtMsg);
end;
procedure TParserDirec.ProcSET;
//Asigna valor a una varaible
var
  varName: String;
  varValue: TDirOperand;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  if lexDir.GetEol then begin
    GenErrorDir(ER_SYNTAX_ERRO);
    exit;
  end;
  if lexDir.GetTokenKind <> lexDir.tnIdentif then begin
    GenErrorDir(ER_IDENT_EXPEC);
    exit;
  end;
  varName :=  lexDir.GetToken;  //lee identificador
  lexDir.Next;
  skipWhites;
  if not CogCarERR('=') then exit;  //sale con error
  varValue := CogExpresion(0);
  if HayError then exit;
  AsigVariable(varName, varValue);
end;
procedure TParserDirec.ProcSET_STATE_RAM;
var
  txtMsg: String;
begin
  lexDir.Next;  //pasa al siguiente
  txtMsg := CogExpresion(0).valStr;
  if HayError then exit;
  pic.SetStatRAMCom(txtMsg);
  if pic.MsjError<>'' then GenErrorDir(pic.MsjError);
end;
procedure TParserDirec.ProcSET_MAPPED_RAM;
var
  txtMsg: String;
begin
  lexDir.Next;  //pasa al siguiente
  txtMsg := CogExpresion(0).valStr;
  if HayError then exit;
  pic.SetMappRAMCom(txtMsg);
  if pic.MsjError<>'' then GenErrorDir(pic.MsjError);
end;
procedure TParserDirec.ProcMAP_RAM_TO_PIN;
{Mapea pines del encapsulado a direcciones de memoria}
var
  txtMsg: String;
begin
  lexDir.Next;  //pasa al siguiente
  txtMsg := CogExpresion(0).valStr;
  if HayError then exit;
  pic.MapRAMtoPIN(txtMsg);
  if pic.MsjError<>'' then GenErrorDir(pic.MsjError);
end;
procedure TParserDirec.ProcSET_UNIMP_BITS;
{Configura bits no implementados para una dirección de RAM.}
var
  txtMsg: String;
begin
  lexDir.Next;  //pasa al siguiente
  txtMsg := CogExpresion(0).valStr;
  if txtMsg='' then begin
    GenErrorDir('Expected string');
  end;
  if HayError then exit;
  pic.SetUnimpBITS(txtMsg);
  if pic.MsjError<>'' then GenErrorDir(pic.MsjError);
end;
procedure TParserDirec.ProcCLEAR_STATE_RAM;
{Limpia el estado de la memoria RAM}
begin
   pic.DisableAllRAM;
end;
procedure TParserDirec.IniExplorDirec(out lin: string);
(*Inicia la exploración del token de directiva. Extrae el delimitador final "}", y
posiciona al lexer justo despues del delimitador inicial "{$". Devuelve la línea
procesada en "lin" (sin delimitador final). Además inicia tokIni
*)
var
  dlin: Integer;
  p: TSrcPos;
begin
  //Fija el inicio del token actual (Esto es válido, porque las directivas son "unilineas")
  tokIni := Cin.curCon.lex.GetX - 1;
  //Usa SynFacilSyn como lexer para analizar texto
  lin := Cin.tok;
  dlin := length(lin);
  if lin[dlin] = '}'  then begin
    delete(lin, dlin, 1);  //quita delimitador final de directiva
  end else begin
    //Es un error, pero es salvable.
    //Ubicamos el error, "manualmente", porque aún no hemos explorado con el lexer.
    p := cIn.ReadSrcPos;
    p.col := tokIni + dlin + 1;  //columna al final
    GenErrorPos(ER_EXPECTED_BR, [], p);
  end;
  //Inicia exploración con el lexer "lexDir"
  lexDir.SetLine(lin, 0);  //inicia cadena
  lexDir.Next;  //Salta el "{$"
  skipWhites;
end;
procedure TParserDirec.ProcDIRline(const AsmLin: string; out ctxChanged: boolean);
{Procesa una directiva, que ha sido definida, para que solo ocupe una sola línea,
para simplificar el procesamiento, ya que si las macros ocupan más de una línea,
complican tremendamente la exploración del lexer y la ubicación de errores.
Sin embargo, las directivas de tipo $IFDEF ... o ELSE ...  se pueden procesar aquí,
leyendo varias líneas sucesivas del código fuente.}
var
  lin, direc: String;
  dmac: TDirMacro;
  p: TSrcPos;
  dvar: TDirVar;
begin
  ctxChanged := false;
  IniExplorDirec(lin);
  if tokType <> lexDir.tnIdentif then begin
    GenErrorDir(ER_ERROR_DIREC);
    exit;
  end;
  //sigue identificador
  case UpperCase(lexDir.GetToken) of
  'PROCESSOR' : ProcPROCESSOR;
  'FREQUENCY' : ProcFREQUENCY;
  'CONFIG'    : ProcCONFIG;
  'INCLUDE'   : ProcINCLUDE(lin, ctxChanged);
  'OUTPUTHEX' : ProcOUTPUTHEX(lin);
  'DEFINE'    : ProcDEFINE(lin);
  'IFDEF'     : ProcIFDEF(lin, false);
  'IFNDEF'    : ProcIFDEF(lin, true);
  'IF'        : ProcIF(lin, false);
  'IFNOT'     : ProcIF(lin, true);
  'ELSE'      : begin
    if WaitForEndIF>0 then begin
      {Estamos dentro de un IF, que se supone dio verdadero, de otra forma, no llegaría
      por aquí. De ser así, el ELSE debe ser falso.}
      cIn.Next;  //toma token {$ELSE}
      //Explora, sin compilar, hasta encontrar directiva delimitadora.
      if not ScanIFDEF(direc) then begin
        //Llegó al final del código fuente, sin encontrar el ENDIF
        GenErrorDir(ER_ENDIF_NOFOU);
        exit;
      end;
      //Encontró token delimitador
      //Si es $ENDIF, no hay problema, todo termina allí, pero si es un else:
      if direc='ELSE' then begin
        GenErrorDir(ER_UNEXP_ELSE);
        exit;
      end;
      //Encontró un $ENDIF
      dec(WaitForEndIF);  //lleva la cuenta
    end else begin
      //No se esperaba
      GenErrorDir(ER_UNEXP_ENDIF);
      exit;
    end;
  end;
  'ENDIF'     : begin
    if WaitForEndIF>0 then begin
      //Se es peraba el delimitador
      Dec(WaitForEndIF);  //para que ya no siga buscando
    end else begin
      //No se esperaba
      GenErrorDir(ER_UNEXP_ENDIF);
      exit;
    end;
  end;
  'MODE'      : ProcMODE;
  'MSGBOX'    : ProcMSGBOX;
  'MSGERR'    : ProcMSGERR;
  'MSGWAR'    : ProcMSGWAR;
  'INFO'      : ProcINFO;
  'WARNING'   : ProcWARNING;
  'ERROR'     : ProcERROR;
  'SET'       : ProcSET;
  'CLEAR_STATE_RAM': ProcCLEAR_STATE_RAM;
  'SET_STATE_RAM'  : ProcSET_STATE_RAM;
  'SET_MAPPED_RAM' : ProcSET_MAPPED_RAM;
//  'SET_RAM_NAME'   : ProcSET_RAM_NAME;
  'MAP_RAM_TO_PIN' : ProcMAP_RAM_TO_PIN;
  'SET_UNIMP_BITS' : ProcSET_UNIMP_BITS;
  else
    //Puede ser una macro
    dmac := FindMacro(lexDir.GetToken);
    if dmac <> nil  then begin
      p := cIn.ReadSrcPos;   //Guarda posición del token
      cIn.Next;  //pasa la directiva
      cIn.NewContextFromTxt(
        dmac.value, //Pasa a explorar contenido de la macro como cadena
        dmac.posDef.fil {Fija el archivo de definiición de la macro.}
      );
      cIn.curCon.autoClose := true;   //Para que se cierre, al finalizar
      cIn.curCon.FixErrPos := true;   //Para que se ignore la posición de los errores
      cIn.curCon.ErrPosition := p;    //Posición a usar para ubicar el error
      cIn.curCon.PreErrorMsg := 'Macro '+dmac.name+': ';
      ctxChanged := true;  //Marca bandera para indciar que se ha cambiado de contexto
    end else if DefinedVar(lexDir.GetToken, dvar) then begin
      //Es variable
      p := cIn.ReadSrcPos;   //Guarda posición del token
      cIn.Next;  //pasa la directiva
      cIn.NewContextFromTxt(
        dvar.valor.valStr, //Pasa a explorar valor de la variable como texto
        '' {Fija el archivo de definiición.}
      );
      cIn.curCon.autoClose := true;   //Para que se cierre, al finalizar
      cIn.curCon.FixErrPos := true;   //Para que se ignore la posición de los errores
      cIn.curCon.ErrPosition := p;    //Posición a usar para ubicar el error
      cIn.curCon.PreErrorMsg := 'Variable '+dvar.nomb+': ';
      ctxChanged := true;  //Marca bandera para indciar que se ha cambiado de contexto
    end else begin
      GenErrorDir(ER_UNKNO_DIREC, [lexDir.GetToken]);
      exit;
    end;
  end;
end;
function TParserDirec.read_PIC_MODEL: string;
begin
  Result := pic.Model;
end;
procedure TParserDirec.write_PIC_MODEL(AValue: string);
begin
  pic.Model := AValue;
end;
function TParserDirec.read_PIC_FREQUEN: Single;
begin
  Result := pic.frequen;
end;
procedure TParserDirec.write_PIC_FREQUEN(AValue: Single);
begin
  pic.frequen := round(AValue);
end;
function TParserDirec.read_PIC_MAXFREQ: Single;
begin
  Result := PIC.MaxFreq;
end;
procedure TParserDirec.write_PIC_MAXFREQ(AValue: Single);
begin
  PIC.MaxFreq := round(AValue);
end;
function TParserDirec.read_PIC_NUMBANKS: Single;
begin
  Result := pic.NumBanks;
end;
procedure TParserDirec.write_PIC_NUMBANKS(AValue: Single);
begin
  pic.NumBanks := round(AValue);
end;
function TParserDirec.read_PIC_NUMPAGES: Single;
begin
  Result := pic.NumPages;
end;
procedure TParserDirec.write_PIC_NUMPAGES(AValue: Single);
begin
  pic.NumPages := round(AValue);
end;
function TParserDirec.read_PIC_IFLASH: Single;
begin
  Result := pic.iFlash;
end;
procedure TParserDirec.write_PIC_IFLASH(AValue: Single);
begin
  pic.iFlash := round(AValue);
end;
function TParserDirec.read_PIC_MAXFLASH: Single;
begin
  Result := pic.MaxFlash;
end;
procedure TParserDirec.write_PIC_MAXFLASH(AValue: Single);
begin
  pic.MaxFlash := round(AValue);
end;
function TParserDirec.read_PIC_NPINS: Single;
begin
  Result := pic.Npins;
end;
procedure TParserDirec.write_PIC_NPINS(AValue: Single);
begin
  pic.Npins := round(AValue);
end;
function TParserDirec.read_SYN_MODE: String;
begin
  case mode of
  modPicPas: Result := 'PicPas';
  modPascal: Result := 'Pascal';
  else
      Result := '';
  end;
end;
procedure TParserDirec.write_SYN_MODE(AValue: String);
begin

end;
function TParserDirec.read_CURRBANK: Single;
begin
  Result := CurrBank;
end;
procedure TParserDirec.write_CURRBANK(AValue: Single);
begin
  CurrBank := Round(AValue);
end;
function TParserDirec.read_CURRBLOCK: String;
var
  parentNod: TxpEleCodeCont;
begin
  if TreeElems.curNode.idClass = eltBody then begin
    //Solo aquí puede haber bloques
    parentNod := TreeElems.CurCodeContainer; //Se supone que nunca debería fallar
    if parentNod.CurrBlock = nil then
      exit('')
    else
      exit(parentNod.CurrBlock.idStr);
  end else begin
    Result := '';
  end;
end;
procedure TParserDirec.ClearMacros;
var
  dvar: TDirVar;
begin
  macroList.Clear;
  WaitForEndIF := 0;
  ConfigWord := -1;   //-1 significa, No Inicializado
  //Limpia variables
  varsList.Clear;
  //Crea  variables del sistema
  //PIC_MODEL
  dvar :=  TDirVar.Create;
  dvar.nomb := 'PIC_MODEL';
  dvar.ReflectToString(@read_PIC_MODEL, @write_PIC_MODEL);
  varsList.Add(dvar);
  //PIC_FREQUEN
  dvar :=  TDirVar.Create;
  dvar.nomb := 'PIC_FREQUEN';
  dvar.ReflectToNumber(@read_PIC_FREQUEN, @write_PIC_FREQUEN);
  varsList.Add(dvar);
  //PIC_MAXFREQ
  dvar :=  TDirVar.Create;
  dvar.nomb := 'PIC_MAXFREQ';
  dvar.ReflectToNumber(@read_PIC_MAXFREQ, @write_PIC_MAXFREQ);
  varsList.Add(dvar);
  //PIC_NUMBANKS
  dvar :=  TDirVar.Create;
  dvar.nomb := 'PIC_NUMBANKS';
  dvar.ReflectToNumber(@read_PIC_NUMBANKS, @write_PIC_NUMBANKS);
  varsList.Add(dvar);
  //PIC_NUMPAGES
  dvar :=  TDirVar.Create;
  dvar.nomb := 'PIC_NUMPAGES';
  dvar.ReflectToNumber(@read_PIC_NUMPAGES, @write_PIC_NUMPAGES);
  varsList.Add(dvar);
  //PIC_IFLASH
  dvar :=  TDirVar.Create;
  dvar.nomb := 'PIC_IFLASH';
  dvar.ReflectToNumber(@read_PIC_IFLASH, @write_PIC_IFLASH);
  varsList.Add(dvar);
  //PIC_NPINS
  dvar :=  TDirVar.Create;
  dvar.nomb := 'PIC_NPINS';
  dvar.ReflectToNumber(@read_PIC_NPINS, @write_PIC_NPINS);
  varsList.Add(dvar);
  //PIC_MAXFLASH
  dvar :=  TDirVar.Create;
  dvar.nomb := 'PIC_MAXFLASH';
  dvar.ReflectToNumber(@read_PIC_MAXFLASH, @write_PIC_MAXFLASH);
  varsList.Add(dvar);

  //SYN_MODE
  dvar :=  TDirVar.Create;
  dvar.nomb := 'SYN_MODE';
  dvar.ReflectToString(@read_SYN_MODE, @write_SYN_MODE);
  varsList.Add(dvar);
  //CurrBank
  dvar :=  TDirVar.Create;
  dvar.nomb := 'CURRBANK';
  dvar.ReflectToNumber(@read_CURRBANK, @write_CURRBANK);
  varsList.Add(dvar);
  //CurrBlock
  dvar :=  TDirVar.Create;
  dvar.nomb := 'CURRBLOCK';
  dvar.ReflectToString(@read_CURRBLOCK, nil);
  varsList.Add(dvar);
end;
constructor TParserDirec.Create;
begin
  inherited Create;
  lexDir := TSynFacilSyn.Create(nil);  //crea lexer para analzar directivas
  macroList := TDirMacro_list.Create(true);
  varsList := TDirVar_list.Create(true);
  DefLexDirectiv;
end;
destructor TParserDirec.Destroy;
begin
  varsList.Destroy;
  macroList.Destroy;
  lexDir.Destroy;
  inherited Destroy;
end;
procedure TParserDirec.DefLexDirectiv;
(*Define la sintaxis del lexer que se usará para analizar las directivas. La que
 debe estar entre los símbolo {$ ... }
*)
begin
  lexDir.ClearSpecials;               //para empezar a definir tokens
  lexDir.CreateAttributes;            //Limpia atributos
  lexDir.ClearMethodTables;

  dirOperator := lexDir.NewTokType('operator');
  dirDelimiter:= lexDir.NewTokType('delimiter');
  lexDir.Attrib[lexDir.tnSymbol].Background := clRed;
  lexDir.Attrib[dirOperator].Background := clGreen;
  //solo se requiere identificadores y números
  lexDir.DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');
  lexDir.DefTokContent('[0-9]', '[0-9.]*', lexDir.tnNumber);
  lexDir.DefTokContent('[$]','[0-9A-Fa-f]*', lexDir.tnNumber);
  lexDir.DefTokDelim('''', '''', lexDir.tnString);  //cadenas
  lexDir.DefTokDelim('"', '"', lexDir.tnString);  //cadenas

  lexDir.AddSymbSpec('=', dirOperator);
  lexDir.AddSymbSpec('+=', dirOperator);
  lexDir.AddSymbSpec('<>', dirOperator);
  lexDir.AddSymbSpec('>', dirOperator);
  lexDir.AddSymbSpec('<', dirOperator);
  lexDir.AddSymbSpec('>=', dirOperator);
  lexDir.AddSymbSpec('<=', dirOperator);

  lexDir.AddIdentSpec('AND', dirOperator);
  lexDir.AddIdentSpec('OR', dirOperator);
  lexDir.AddIdentSpec('XOR', dirOperator);
  lexDir.AddIdentSpec('NOT', dirOperator);

  lexDir.AddSymbSpec('+', dirOperator);
  lexDir.AddSymbSpec('-', dirOperator);
  lexDir.AddSymbSpec('*', dirOperator);
  lexDir.AddSymbSpec('/', dirOperator);
  lexDir.AddSymbSpec('\', dirOperator);
  lexDir.AddSymbSpec('%', dirOperator);
  lexDir.AddSymbSpec('^', dirOperator);

  lexDir.AddSymbSpec('(', dirDelimiter);
  lexDir.AddSymbSpec(')', dirDelimiter);
  lexDir.AddSymbSpec('{$', dirDelimiter);
  lexDir.AddSymbSpec('}', dirDelimiter);
  lexDir.Rebuild;
end;

end.
