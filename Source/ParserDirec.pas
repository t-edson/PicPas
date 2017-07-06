{Unidad que implementa a la clase TParserDirec, que sirve como contenedor para
implementar las funcionaliddes de procesamiento de directivas.
}
unit ParserDirec;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, ParserAsm, fgl, SynFacilHighlighter, Globales,
  Pic16Devices, MisUtils, XpresBas;
type
  //Identifica a una macro
  TPicMacro = class
    name: string;
    value: string;
  end;

  TPicMacro_list = specialize TFPGObjectList<TPicMacro>;

  { TParserDirec }
  TParserDirec = class(TParserAsm)
  private
    lexDir : TSynFacilSyn;  //lexer para analizar directivas
    dirOperator: Integer;
    dirDelimiter: integer;
    procedure GenErrorDir(msg: string; const Args: array of const);
    procedure GenErrorDir(msg: string);
    function modeStr: string;
    procedure ProcCONFIG;
    procedure ProcFREQUENCY;
    procedure ProcDEFINE(lin: string);
    procedure ProcMODE;
    procedure ProcMSGBOX;
    procedure ProcPROCESSOR;
    procedure skipWhites;
    function GetIdent: string;
    function tokType: integer;
    procedure DefLexDirectiv;
  protected //Variables internas del compilador
    mode      : (modPascal, modPicPas);
    macroList : TPicMacro_list;
  protected
    procedure NewMacro(macName, macValue: string);
    procedure ProcDIRline(const AsmLin: string; xIni: integer);
  public
    Compiling : boolean;  //Bandera para el compilado
    procedure ClearMacros;
    constructor Create; override;
    destructor Destroy; override;
  end;

  procedure SetLanguage(idLang: string);

implementation
var
  ER_ERROR_DIREC, ER_UNKNO_DEVIC, ER_MODE_UNKNOWN, ER_UNKNO_DIREC,
  ER_ERROR_FREQ, ER_IDENT_EXPEC, ER_EXPEC_EQUAL,
  ER_SYNTAX_ERRO, ER_SYNTAX_ERR_: string;

procedure SetLanguage(idLang: string);
begin
  curLang := idLang;
  {$I ..\language\tra_ParserDirec.pas}
end;

{ TParserDirec }
procedure TParserDirec.GenErrorDir(msg: string);
{Genera un error corrigiendo la posición horizontal}
var
  p: TSrcPos;
begin
  p := cIn.ReadSrcPos;
  p.col := lexDir.GetX;  //corrige columna, considera los caracteres eliminados
  GenErrorPos(msg, [], p);
end;
procedure TParserDirec.GenErrorDir(msg: string; const Args: array of const);
var
  p: TSrcPos;
begin
  p := cIn.ReadSrcPos;
  p.col := lexDir.GetX;  //corrige columna, considera los caracteres eliminados
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
procedure TParserDirec.NewMacro(macName, macValue: string);
{Agrega una nueva macro a la lista de macros}
var
  mac: TPicMacro;
begin
  mac := TPicMacro.Create;
  mac.name := macName;
  mac.value := macValue;
  macroList.Add(mac);
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
  pic.frequen:=f; //asigna freecuencia
end;
procedure TParserDirec.ProcCONFIG;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;


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
  txtMsg, tmp: String;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  //Captura tokens para mostrarlos
  txtMsg := '';
  while not lexDir.GetEol do begin
    tmp :=  lexDir.GetToken;  //lee token
    if lexDir.GetTokenKind = lexDir.tnString then begin
      tmp := copy(tmp, 2, length(tmp)-2);
//      end else if lexDir.GetToken = '}' then begin
//        break;  //sale
    end else begin
      //Verifica si es variable de sistema
      if UpCase(tmp) = '$FREQUENCY' then tmp := IntToStr(pic.frequen);
      if UpCase(tmp) = '$PROCESSOR' then tmp := pic.Model;
      if UpCase(tmp) = '$MODE' then tmp := modeStr;
      if UpCase(tmp) = '$CURRBANK' then tmp := IntToStr(CurrBank);
      if UpCase(tmp) = '$IFLASH' then tmp := IntToStr(pic.iFlash);
    end;
    //Concatena
    txtMsg := txtMsg + tmp;
    lexDir.Next;  //pasa al siguiente
    skipWhites;
  end;
  //Solo muestra en compilación y en la primera pasada
  if Compiling and FirstPass then msgbox(txtMsg);
end;
procedure TParserDirec.ProcDIRline(const AsmLin: string; xIni: integer);
var
  lin: String;
  dlin: Integer;
begin
  //Usa SynFacilSyn como lexer para analizar texto
  lin := Cin.tok;
  dlin := length(lin);
  if lin[dlin]= '}'  then begin
    delete(lin, dlin, 1);  //quita delimitador final de directiva
  end;
  lexDir.SetLine(lin, 0);  //inicia cadena
  lexDir.Next;  //Salta el "{$"
  skipWhites;
  if tokType <> lexDir.tnIdentif then begin
    GenErrorDir(ER_ERROR_DIREC);
    exit;
  end;
  //sigue identificador
  case UpperCase(lexDir.GetToken) of
  'PROCESSOR': ProcPROCESSOR;
  'FREQUENCY': ProcFREQUENCY;
  'CONFIG'   : ProcCONFIG;
  'DEFINE'   : ProcDEFINE(lin);
  'MODE'     : ProcMODE;
  'MSGBOX'   : ProcMSGBOX;
  else
    GenErrorDir(ER_UNKNO_DIREC, [lexDir.GetToken]);
    exit;
  end;
end;
procedure TParserDirec.ClearMacros;
begin
  macroList.Clear;
end;
constructor TParserDirec.Create;
begin
  inherited Create;
  lexDir := TSynFacilSyn.Create(nil);  //crea lexer para analzar directivas
  macroList := TPicMacro_list.Create(true);
  DefLexDirectiv;
end;
destructor TParserDirec.Destroy;
begin
  macroList.Destroy;
  lexDir.Destroy;
  inherited Destroy;
end;
procedure TParserDirec.DefLexDirectiv;
(*Define la sintaxis del lexer que se usará para analizar las directivas. La que
 debe estar entre los símbolo {$ ... }
*)
begin
  dirOperator := lexDir.NewTokType('operator');
  dirDelimiter:= lexDir.NewTokType('delimiter');
  //solo se requiere identificadores y números
  lexDir.DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');
  lexDir.DefTokContent('[0-9]', '[0-9.]*', lexDir.tnNumber);
  lexDir.DefTokDelim('''', '''', lexDir.tnString);  //cadenas
  lexDir.AddSymbSpec('=', dirOperator);
  lexDir.AddSymbSpec('{$', dirDelimiter);
  lexDir.AddSymbSpec('}', dirDelimiter);
  lexDir.Rebuild;
end;

end.

