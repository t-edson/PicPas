{Unidad que implementa a la clase TParserDirec, que sirve como contenedor para
implementar las funcionaliddes de procesamiento de directivas.
}
unit ParserDirec_PIC10;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, math, Graphics, LCLProc, SynFacilHighlighter, XpresBas,
  XpresElementsPIC, Pic10Devices, ParserAsm_PIC10, Globales, MisUtils;
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
    function CogIdentif(out s: string): boolean;
    function CogNumero(var n: Double): boolean;
    function CogOperando: TDirOperand;
    function cogOperador: String;
    function DefinedVar(cad: string; out dvar: TDirVar): boolean;
    procedure ProcINFO;
    procedure ProcMSGERR;
    procedure ProcMSGWAR;
    procedure ProcSET_MAPPED_RAM;
    procedure ProcRESET_PINS;
    procedure ProcSET_PIN_NAME;
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
    function read_PIC_ENHANCED: Single;
    procedure write_PIC_ENHANCED(AValue: Single);
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
    mode      : (modPascal, modPicPas);
    ConfigWord: integer;  //Bits de configuración
    procedure NewMacro(macName, macValue: string);
    procedure ProcDIRline(const AsmLin: string; out ctxChanged: boolean);
    function DefinedMacro(macName: string): boolean;
    function FindMacro(macName: string): TDirMacro;
  public
    procedure ClearMacros;
    constructor Create; override;
    destructor Destroy; override;
  end;

  procedure SetLanguage;

implementation
var
  ER_ERROR_DIREC, ER_UNKNO_DEVIC, ER_MODE_UNKNOWN, ER_UNKNO_DIREC,
  ER_ERROR_FREQ, ER_IDENT_EXPEC, ER_EXPEC_EQUAL,
  ER_SYNTAX_ERRO, ER_SYNTAX_ERR_: string;
  ER_EXPECTED_BR, ER_ENDIF_NOFOU, ER_UNEXP_ENDIF: String;
  ER_UNEXP_ELSE, ER_CONF_UNDEF_, ER_INVAL_CBIT_: String;
  ER_FILE_NO_FND_, ER_ERIN_NUMBER_, ER_UNKNW_IDENT_: String;
  ER_DIVIDE_ZERO, ER_EVA_ZER_ZER, ER_OPE_NOT_IMP_: String;
  ER_EXPECT_CAR_: String;
  ER_TOOHIGHFRE: String;
procedure SetLanguage;
begin
  ParserAsm_PIC10.SetLanguage;
{$I ..\language\tra_ParserDirec.pas}
end;
{ TDirOperand }
{$I .\ParserDirec.inc}
//Otras variables
function TParserDirec.read_PIC_ENHANCED: Single;
begin
  if pic.Enhanced then Result := 1 else Result := 0;
end;
procedure TParserDirec.write_PIC_ENHANCED(AValue: Single);
begin
  pic.Enhanced := (round(AValue) <> 0);
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
  //Enhanced
  dvar :=  TDirVar.Create;
  dvar.nomb := 'PIC_ENHANCED';
  dvar.ReflectToNumber(@read_PIC_ENHANCED, @write_PIC_ENHANCED);
  varsList.Add(dvar);
end;

end.
