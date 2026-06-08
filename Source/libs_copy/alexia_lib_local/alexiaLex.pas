{Pascal lexer, using the language defined in the compilers PicPas and P65pas.
                                          Created by Tito Hinostroza  08/03/2020
}
unit alexiaLex;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, Lazlogger;
type
 TString2 = string[2];
  //Context types
  tTypCon = (
    TC_ARC,     //File type context.
    TC_TXT      //Text type context.
  );

  //Primary location for elements
  { TODO : ¿Debe estar aquí? Formalmente esta clasific. pertenece al nivel de elementos. }
  TElemLocation = (
                locMain,       //En el programa principal.
                locInterface,  //En INTERFACE de una unidad.
                locImplement   //En IMPLEMENTATION de una unidad.
  );

  TTokenKind = (
    //Pascal Lexer token
    tkNull     ,  //Not defined token. Single-line token.
    tkEol      ,  //End of line. Single-line token.
    tkSymbol   ,  //Symbol
    tkSpace    ,  //Space token. Consider only characters #9 and #32. Single-line token.
    tkIdentifier, //Identifier. Single-line token.
    tkLitNumber,  //Literal number
    tkString   ,  //Literal String
    tkComment  ,  //Comment. Multi-line token.
    tkOperator ,  //Operators
    tkDirective,  //
//    tkAsm      ,  //
    tkExpDelim ,  //
    tkBlkDelim ,  //
    tkChar     ,  //
    tkKeyword  ,  //Reserved words
    tkDirDelim ,  //Delimitador de directiva. Usado solo para directivas.
    tkOthers      //
  );

  { TScanner }
  {Define a basic lexer that can explore source text, character form character.
  This lexer only can read chars and identifies special positions of the
  source text.
  Attribute "curLines" is a reference to the source text.
  Source text is viewed as a list of lines.
  Each line is considered as a set of characters and a additional next-line character.
  So a common line like "HOLA", is mapped as:
  +---+---+---+---+---+
  | 1 | 2 | 3 | 4 | 5 |
  +---+---+---+---+---+
  | H | O | L | A |#13|
  +---+---+---+---+---+
                    |
                   EOL

  A final line like "HI", is mapped as:
  +---+---+---+
  | 1 | 2 | 3 |
  +---+---+---+
  | H | I |#13|
  +---+---+---+
            |
         EOL,EOF

  The initial position for cursor of the scanner is:
      (frow=1, fcol=1).
  The ending position is:
      (frow=nlin, fcol=length_last_line)
  ReadChar() can returns:
   - A valid character of the text.
   - A empty string, if the cursor points to a EOL character or the source text is empty.
  }
  TScanner = class
  private
    {Attributes for store the state of the scanner. Used to restore a position of the
    scanner after reading chars. Note that the source text, and related information
    is not saved, only the scanner cursor position. }
    frow_    : integer;
    fcol_    : integer;
    curLine_ : string;
    curSize_ : integer;
  public  //Fast versions of some functions.
    function _Bol: boolean; inline;
    function _Eol: boolean; inline;
    function _Eof: boolean; inline;
    function _FirstLine: boolean; inline;
    function _LastLine: boolean; inline;
    procedure _setRow(row0: integer); inline;
    procedure _setCol(col0: integer); inline;
    function _ReadChar: char; inline;
    function _Read2Char: TString2; inline;
    procedure _NextChar; inline;
    procedure _setEOL;      //Set cursor at the end of current line
  public
    //Cursor position.
    frow     : integer;    //From 1 to "nlin".
    fcol     : integer;    //From 1 to "curSize".
    //Current information
    curLine  : string;     //Stores the current line. Depends on "frow".
    curSize  : integer;    //Length of the current line. Depends on "curLine". It's value is length(curLine)+1, because it consider the \n as a one more character.
    //Information of the source text.
    curLines : TStrings;   //Reference to the current StringList, used to scan. This class doesn't have a container for the input text.
    nlin     : LongInt;    //Number of lines of the source.
  public  //Scanner state
    procedure SaveScannerState;
    procedure RestoreScannerState;
  public  //Check positions
    function Empty: boolean; inline;  //Returns TRUE if ther is not source text.
    function Bol: boolean;  //Returns TRUE if cursor is at the begin of the current line.
    function Eol: boolean;  //Returns TRUE if cursor is at the end of the current line.
    function FirstLine: boolean;
    function LastLine: boolean;
    function Bof: boolean; inline; //Returns TRUE if cursor is at the begin of file.
    function Eof: boolean; inline; //Returns TRUE if cursor is at the end of file.
  public  //Set positions
    procedure setRow(row0: integer);  //Set the current row
    procedure setCol(col0: integer);  //Set the current col
    procedure setEOL;      //Set cursor at the end of current line
    procedure setBOF;      //Set cursor at the begin of file
    procedure setEOF;      //Set cursor at the end of file
  public  //Scan functions
    procedure NextChar;        //Move cursor to the next position
    function ReadChar: char; inline;  //Returns the char pointed by cursor
    procedure SetText(strList: TStringList);  //Sets the source text.
    //constructor Create;
    //destructor Destroy; override;
  end;

  { TSrcPos }
  { Position in the source code. Used to:
   - Locate syntax elements in source code.
   - Define positions for errors or warnings. Only position, because the complete
     information for messages are stored in a "TMsgInfo" record.
  As all the syntax elements must have a TSrcPos field, the size of this object
  should be kept small.
  This position can also be used to return the scanner to a specific state of
  exploration, but not to restore the lexer position, because lexer uses aditional
  information. To save and restore the position of the lexer (TAleLexer), it should be
  used the "TContextState" record.
  However, restoring a position with TSrcPos, can work after doing a next() call in the
  lexer. }
  TSrcPos = object
    //Id for the context. Through this reference we can obtain information about the file.
    idCtx  : integer;
    //Attributes for position.
    row    : integer;  //Row number
    col    : integer;  //Column number
    function RowColString: string;
    function EqualTo(const target: TSrcPos): boolean;
  end;
  TSrcPosArray = array of TSrcPos;
type  //Gestor de mensajes
  //Information about a position on the source code.
  //Used to send messages to the message manager.
  TMsgInfo = object
    txt    : string;   //Message text
    fname  : string;   //SOurce file
    row    : integer;  //Row number
    col    : integer;  //Column number
  end;
  //Types of messages generated by the compiler
  TMessageKind = (
    mkInfo,     //Mensajes informativos
    mkWarning,  //Mensajes de advertencia
    mkError     //Mensajes de error
  );
  { TMessageManager }
  {Gestor de mensajes para el lexer y para el compilador también. Se crea como una clase
  separada para centralizar la generación de mensajes y permitir así una fácil
  integración del compilador con una IDE (donde pueden haber otros compialdores) o con
  una consola}
  TMessageManager = class
  public  //Información sobre los mensajes
    //Número de errores generados
    nErrors: Integer;          //Número de errores generados
    nWarns : Integer;          //Número de adevrtencias generadas
    nInfos : Integer;          //Número de mensajes de información
    function txtNWarnings: String; //Texto descriptivo de la cantidad de advertencias.
    function txtNErrors: String;   //Texto descriptivo de la cantidad de errores.
  public  //Manejo de mensajes de consola
    //Evento que indica que se ha generado un mensaje (Info, Warning or Error)
    OnMessage: procedure(msgKind: TMessageKind; const msgInfo: TMsgInfo) of object;
    //Evento que indica que se desea generar un mensaje por cuadro de diálogo.
    //El parámetro "mode" indica el tipo de mensaje:
    //  0->Mensaje normal, 1->Mensaje de advertencia, 2->Mensaje de error
    OnMessageBox: procedure(txt: string; mode: integer) of object;
    procedure info(const msgInfo: TMsgInfo);
    procedure warn(const msgInfo: TMsgInfo);
    procedure error(const msgInfo: TMsgInfo);
  public  //Manejo de mensajes en cuadros de diálogos

    procedure msgbox(const txt: string);
    procedure msgwar(const txt: string);
    procedure msgerr(const txt: string);
  end;
type //Clase "TContext"
  { TContextState }
  {Record for storing the position and state of the lexer. Used to save and restore the
  position of the lexer (and the same state) after reading more tokens.
  Successful position restoration implies that the source text has not changed. }
  TContextState = record
    idCtx   : integer;     //Id for the context.
    //Attributes of TScannerState
    frow    : integer;
    fcol    : integer;
    curLine : string;
    curSize : integer;
    //Atributes of TContext
    row0    : integer;
    col0    : integer;
    tokType : TTokenKind;
    tokPrec : integer;
    tokPrecU: integer;    //Precedence when "tokType" is "tkOperator" and operator can be used as unary operator.
  end;

  { TContext }
  {Estructura que define a un objeto contexto. Un contexto es un objeto que sirve como
  entrada de datos, en donde se puede cargar un texto, y luego leerlo token por token
  de forma simple.}
  TContext = class(TScanner)
  private
    fLexerState: TContextState;  //almacenamiento temporal
  public  //State of the scanner
    //Position for start of current token
    row0     : integer;    //From 1 to "nlin". ?
    col0     : integer;    //From 1 to "curSize". Set to 0 at the beginning. ?
    //Information for current token
    tokType  : TTokenKind; //Current token kind.
    tokPrec  : integer;    //Precedence when "tokType" is "tkOperator".
    tokPrecU : integer;    //Precedence when "tokType" is "tkOperator" and operator can be used as unary operator.
    //The rest of the state are defined by TScanner: frow, fcol, curLine, ...
  public  //Control for current position
    procedure GetContextState(out c: TContextState);
    procedure SetContextState(const c: TContextState);
    procedure SaveContextState;    //Guarda el estado actual del lexer
    procedure RestoreContextState; //Restaura el estado actual del lexer
    //Current cursor position.
    property row: integer read frow;
    property col: integer read fcol;
  public  //Containers for content
    intLines : TStringList;  {Internal containers for text, when not specified an external
                             TStringList. Always created.}
  private  //Scan functions
    function DecodeNext: boolean;
  public  //Scan functions
    OnDecodeNext: function: boolean of object;
    function Next: boolean;      //Pasa al siguiente token
    function ReadToken: string;  //Returns the current token
    function tokTypeStr: string;
    procedure SkipWhites;
    procedure SkipWhitesNoEOL;
  public
    idCtx    : integer;     //Unique identifier for the context.
    retPos   : TContextState; //Return position to parent context.
    typ      : tTypCon;     //Context type.
    fileSrc  : String;      //Nombre de archivo fuente, incluyendo la ruta. En caso de que el contexto corresponda a uno.
    data     : TObject;     //Campo para información adiciconal que se desee almacenar.
    autoReturn: boolean;    {Indica que, al finalizar la exploración, se debe retornar al
                            contexto que hizo la llamada.}
    autoRemove: boolean;    {Indica que, al finalizar la exploración, se debe eliminar
                            este contexto. Solo es válido cuando se activa "autoReturn".}
    function ReadSource: string;    //Lee el contenido del contexto en un string
  public  //Error generation
    onErrorScan: procedure(msg: string) of object;  //Generates scanner error
  public  //Manage of the Pre-error.
    {The Pre-error is a technique that affect the generation of errors in this context.
    When it's activated, it means that if an error is generated when scannig this context,
    the error will be located at the position indicated by "PreErrPosit", instead of the
    normal position.
    }
    FixErrPos: boolean;     {Indica que los mensajes de error, deben apuntar a una
                             posición fija, y no a la posición en donde se detecta el error.}
    PreErrPosit: TSrcPos;  //Posición a usar para el error, cuando se activa FixErrPos.
    PreErrorMsg: string;    {Mensaje previo al mensaje de error, cuando el errror se
                             genere en este contexto. Como se va a concatenar con otro
                             mensaje de error, debería terminar en ": " o ". ".}
  public  //Métodos de inicialización
    function IniCont:Boolean;
    procedure StartScan;
    procedure SetSource(txt : string);   //Fija el contenido del contexto con cadena
    procedure SetSource(lins: Tstrings; MakeCopy: boolean = false); //Fija contenido a partir de una lista
    procedure SetSourceF(file0: string);  //Fija el contenido del contexto con archivo
    constructor Create;
    destructor Destroy; override;
  end;

  //Define una lista de Contextos
  TContextList = specialize TFPGObjectList<TContext>;
type  //Lexer TAleLexer
  { TAleLexer }
  //Extructura para manejar diversas fuentes de datos de contexto
  TAleLexer = class
  private
    idCount  : integer;        //Contador para obtener el índice de un contexto
    procedure AleLexerErrorScan(txt: string);
  public  //Events
    OnNewLine: procedure(lin: string) of object;
  public //Information for current context
    curCtx   : TContext;       //referencia al contexto de entrada actual
    //Control for state
    function GetCtxState: TContextState;
    procedure SetCtxState(pc: TContextState);
    //Control for position
    function GetSrcPos: TSrcPos; inline;
    procedure SetSrcPos(const srcPos: TSrcPos);
    function GetMsgInfo(const txt: string): TMsgInfo;
    function GetMsgInfo(txt: string; const srcPos: TSrcPos): TMsgInfo; inline;
    function GetMsgInfoE(const txt: string): TMsgInfo;
    function GetMsgInfoE(const txt: string; const srcPos: TSrcPos): TMsgInfo;
  public //Information for any context
    function ctxId(fileSrc: string): integer;
    function ctxFile(idCtx: integer): string;
    function ctxFile(const srcPos: TSrcPos): string;
    function ctxFileName(const srcPos: TSrcPos): string;
    function ctxFileDir(const srcPos: TSrcPos): string;
  protected  //Context manage
    ctxList: TContextList;   //Lista de contextos de entrada
    function AddContext: TContext;
    procedure NewContextFromFile(filSrc: String; out notFound: boolean);
    procedure NewContextFromTStrings(lins: Tstrings; filSrc: String);
    procedure ReturnToPrevContext(remove: boolean);
  public
    {Indica que se va a acceder a un archivo para crear un contexto, pero se está
    preguntando si se tiene un Stringlist, con los datos ya cargados del archivo, para
    evitar tener que abrir nuevamente al archivo desde disco.}
    OnRequireFileString: procedure(FilePath: string; var strList: TStrings) of object;
    procedure ClearContexts;      //Deletes all contexts.
    procedure NewContextFromText(txt: string; fileSrc: String);
    function OpenContextFrom(filePath: string): boolean;
  public //Scan functions
    token    : string;     //Current Token
    tokType  : TTokenKind; //Current Token type
    function tokL: string; inline; //token actual en minúscula
    function atEol: Boolean; inline;
    function atEof: Boolean;
    procedure SkipWhites;
    procedure SkipWhitesNoEOL;
    procedure Next;       //Go to the next token.
    procedure GotoEOL;    //Go to the EOL position.
  public  //Context debugging
    procedure ShowContexts;
    procedure ShowCurContInformat;
  public    //Errors and warnings
    msg: TMessageManager;        //Referencia al gestor de mensajes
    //Variables
    curLocation: TElemLocation;  {Current location for scan. This tells the compiler
                                  where it's scanning. It useful because some declarations
                                  have to interpret in different way according to the
                                  location.}
  public  //Initialization
    constructor Create(msg0: TMessageManager);
    destructor Destroy; override;
  end;

var
  srcPosNull: TSrcPos;  //Object TScrcPos NULL.
resourcestring
  MSG_WARN    = 'Warning'       ;   //Singular
  MSG_WARNS   = 'Warnings'      ;   //Plural
  MSG_ERROR   = 'Error'         ;   //Singular
  MSG_ERRORS  = 'Errors'        ;   //Plural

implementation
{ TMessageManager }
function TMessageManager.txtNWarnings: String;
begin
  if nWarns = 1 then begin
    Result := '1 ' + MSG_WARN;
  end else begin
    Result := IntToStr(nWarns) + ' ' + MSG_WARNS;
  end;
end;
function TMessageManager.txtNErrors: String;
begin
  if nErrors = 1 then begin
    Result := '1 ' + MSG_ERROR;
  end else begin
    Result := IntToStr(nErrors) + ' ' + MSG_ERRORS;
  end;
end;
procedure TMessageManager.info(const msgInfo: TMsgInfo);
begin
  inc(nInfos);
  if Assigned(OnMessage) then OnMessage(mkInfo, msgInfo);
end;
procedure TMessageManager.warn(const msgInfo: TMsgInfo);
begin
  inc(nWarns);
  if Assigned(OnMessage) then OnMessage(mkWarning, msgInfo);
end;
procedure TMessageManager.error(const msgInfo: TMsgInfo);
begin
  inc(nErrors);
  if Assigned(OnMessage) then OnMessage(mkError, msgInfo);
end;
procedure TMessageManager.msgbox(const txt: string);
{Muestra un diñalogo con un mensaje normal}
begin
  if Assigned(OnMessageBox) then
    OnMessageBox(txt, 0);
end;
procedure TMessageManager.msgwar(const txt: string);
{Muestra un diñalogo con un mensaje de advertencia}
begin
  if Assigned(OnMessageBox) then
    OnMessageBox(txt, 1);
end;
procedure TMessageManager.msgerr(const txt: string);
{Muestra un diñalogo con un mensaje de error}
begin
  if Assigned(OnMessageBox) then
    OnMessageBox(txt, 2);
end;

{ TScanner }
//Fast versions of some functions.
function TScanner._Bol: boolean;
begin
  exit(fcol = 1);
end;
function TScanner._Eol: boolean;
{Basic version of Eol() with no validation.}
begin
  exit(fcol = curSize);  //TRUE also at EOF.
end;
function TScanner._Eof: boolean;
begin
  exit(_LastLine and _Eol);
end;
function TScanner._FirstLine: boolean;
{Basic version of FirstLine() with no validation.}
begin
  exit(frow=1);
end;
function TScanner._LastLine: boolean;
//Basic version of _LastLine().
begin
  exit(frow=nlin);
end;
procedure TScanner._setRow(row0: integer);
{Basic version of setRow() with no validations.}
begin
  //Set current row, updating related attributes.
  frow := row0;
  curLine := curLines[frow-1];
  curSize := length(curLine)+1;  //Consider an extra char for the EOL, even in the last line.
end;
procedure TScanner._setCol(col0: integer);
{Basic version of setCol() with no validation.}
begin
  fcol := col0;
end;
procedure TScanner._NextChar;
begin
  if _EOL then begin
    //At the end of the current line.
    if _LastLine then begin  //The last line
      //Cannot advance to a NextChar line. Keep position (EOF)
    end else begin
      //There is a NextChar line
      _setRow(frow+1);
      _setCol(1);
    end;
  end else begin
    //A normal position
    inc(fcol);
  end;
end;
procedure TScanner._setEOL;
begin
  fcol := curSize;
end;
function TScanner._ReadChar: char;
begin
  if _Eol then exit(#13);  //End of line
  exit(curLine[fcol]);    //Common character
end;
function TScanner._Read2Char: TString2;
{Return two character starting from the current position. If there isn't
two character in the current position (like in the EOF) returns the string '  '.}
begin
  if fcol >= curSize-1 then exit('  ');
  exit(curLine[fcol] + curLine[fcol+1]);
end;
//Scanner state
procedure TScanner.SaveScannerState;
begin
  frow_    := frow;
  fcol_    := fcol;
  curLine_ := curLine;
  curSize_ := curSize;
end;
procedure TScanner.RestoreScannerState;
begin
  frow    := frow_;
  fcol    := fcol_;
  curLine := curLine_;
  curSize := curSize_;
end;
function TScanner.Empty: boolean;
begin
  exit(nlin=0);
end;
//Check positions
function TScanner.Bol: boolean;
begin
  if Empty then exit(true);
  exit(_Bol);
end;
function TScanner.Eol: boolean;
{Returns TRUE is the cursor is located on the end of the current line.}
begin
  if Empty then exit(false);  //Empty file.
  exit (_Eol);  //TRUE also at EOF.
end;
function TScanner.FirstLine: boolean;
begin
  if Empty then exit(false);  //Empty file.
  exit(_FirstLine);
end;
function TScanner.LastLine: boolean;
begin
  if Empty then exit(false);  //Empty file.
  exit(_LastLine);
end;
function TScanner.Bof: boolean;
begin
  if Empty then exit(true);  //Empty file.
  exit(_FirstLine and _Bol);
end;
function TScanner.Eof: boolean;
{Returns TRUE if the cursor is located to the end of the source or the source
is empty.}
begin
  if Empty then exit(true);
  exit(_LastLine and _Eol);
  {Other opcion would be (frow>nlin) however it's not possible by definition, except
  when source is Empty() (already checked).}
end;
//Set positions
procedure TScanner.setRow(row0: integer);
begin
  //Check for empty source text
  if Empty then begin
    frow := 1;
    exit;
  end;
  //Validate limit
  if row0>nlin then row0 := nlin;
  //Set row
  _setRow(row0);
end;
procedure TScanner.setCol(col0: integer);
begin
  //Check for empty source text
  if Empty then begin
    fcol := 1;
    exit;
  end;
  //Validate limit
  if col0>curSize then col0 := curSize;
  //Set current column
  _setCol(col0);
end;
procedure TScanner.setEOL;
begin
  if Empty then exit;  //Empty file.
  _setEOL;
end;
procedure TScanner.setBOF;
begin
  _setRow(1);
  _setCol(1);
end;
procedure TScanner.setEOF;
begin
  if Empty then setBOF; //Empty source text
  _setRow(nlin);     //Point to the last line
  _setCol(curSize);  //Point to an nonexistent "\n".
end;
//Scan functions
procedure TScanner.NextChar;
{Move cursor to next position.}
begin
  if Empty then exit;  //Empty source
  _NextChar;
end;
function TScanner.ReadChar: char;
begin
  if Empty then exit(#0); //Empty source
  exit(_ReadChar);    //Common character
end;
procedure TScanner.SetText(strList: TStringList);
begin
  curLines := strList;   //Just copy the reference.
end;
{ TSrcPos }
function TSrcPos.RowColString: string;
begin
  Result := '[' + IntToStr(Row) + ',' + IntToStr(Col)+']';
end;
function TSrcPos.EqualTo(const target: TSrcPos): boolean;
begin
  Result := (idCtx = target.idCtx) and
            (row    = target.row) and
            (col    = target.col);
end;
{ TContext }
function TContext.IniCont: Boolean;
//Devuelve verdadero si se está al inicio del Contexto (fila 1, columna 1)
begin
  Result := (row = 1) and (col = 1);
end;
procedure TContext.SkipWhites;
//Coge los blancos iniciales, saltos de línea y comentarios del contexto de entrada.
//Si no encuentra algun blanco al inicio, devuelve falso
begin
  while toktype in [tkSpace , tkEol, tkComment] do
  begin
    DecodeNext;
  end;
end;
procedure TContext.SkipWhitesNoEOL;
//Get initial whites from input context. Doesn't consider EOL as white.
//If not whites are found, returns FALSE.
begin
  while toktype = tkSpace do begin
    DecodeNext;
  end;
end;
function TContext.DecodeNext: boolean;
{Decode the token in the current position, indicated by (frow, fcol), and returns:
 - Token type in "tokType".
 - Token precedence in "tokPrec", when "tokType" is "tkOperator".
 - Start of next token in (frow, fcol).
 - Value TRUE if the current line has changed.

 Token precedence is the common in Pascal:
 6)    ~, not, unary "+", unary "-", @, **  (high precedence)
 5)    *, /, div, mod, and, shl, shr, &
 4)    |, !, +, -, or, xor
 3)    =, <>, <, <=, >, >=, in
 2)    :=, +=, -=, *=, /=  (low precedence)

}

var
  iden: String;
begin
  if _Eof then begin
    tokType := tkNull;
    exit(false);
  end else if _Eol then begin
    tokType := tkEol;
    if _LastLine then begin
      //Cannot advance to a NextChar line. Keep position (EOF)
    end else begin
      //In a common line
      _setRow(frow+1);
      _setCol(1);
    end;
    exit(true);
  end;
  case curLine[fcol] of
  #32, #9: begin
    repeat
      inc(fcol);
    until _Eol or not(curline[fcol] in [#32, #9]);
    tokType := tkSpace;
    //Leaves (frow, fcol) in the begin of the next token.
  end;
  '0'..'9': begin
    repeat
      inc(fcol);
    until _Eol or not(curline[fcol] in ['0'..'9']);
    tokType := tkLitNumber;
  end;
  '$': begin
    repeat
      inc(fcol);
    until _Eol or not(curline[fcol] in ['0'..'9','A'..'F','a'..'f']);
    tokType := tkLitNumber;
  end;
  '%': begin
    repeat
      inc(fcol);
    until _Eol or not(curline[fcol] in ['0','1']);
    tokType := tkLitNumber;
  end;
  'A','a': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if iden = 'AND' then begin
      tokType := tkOperator;
      tokPrec := 5;
    end else if iden = 'ARRAY' then begin
      tokType := tkKeyword;
    end else if iden = 'ASM' then begin
      tokType := tkKeyword;
    end else begin
      tokType := tkIdentifier;
    end;
  end;
  'B','b': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if iden = 'BEGIN' then begin
      tokType := tkKeyword;
    end else begin
      tokType := tkIdentifier;
    end;
  end;
  'C','c': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if iden = 'CASE' then begin
      tokType := tkKeyword;
    end else if iden = 'CONST' then begin
      tokType := tkKeyword;
    end else begin
      tokType := tkIdentifier;
    end;
  end;
  'D','d': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'DIV') then begin
      tokType := tkOperator;
      tokPrec := 5;
    end else tokType := tkIdentifier;
  end;
  'E','e': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'END') or (iden='ELSE') or (iden='ELSIF') then tokType := tkBlkDelim
    else tokType := tkIdentifier;
  end;
  'I','i': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'IN') then begin
      tokType := tkOperator;
      tokPrec := 3;
    end else if iden = 'INTERFACE' then begin
      tokType := tkKeyword;
    end else if iden = 'IMPLEMENTATION' then begin
      tokType := tkKeyword;
    end else tokType := tkIdentifier;
  end;
  'M','m': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'MOD') then begin
      tokType := tkOperator;
      tokPrec := 5;
    end else tokType := tkIdentifier;
  end;
  'N','n': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'NOT') then begin
      tokType := tkOperator;
      //tokPrec := 6;
      tokPrecU := 6;
    end else tokType := tkIdentifier;
  end;
  'O','o': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'OR') then begin
      tokType := tkOperator;
      tokPrec := 4;
    end else tokType := tkIdentifier;
  end;
  'P','p': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'PROCEDURE') then begin
      tokType := tkKeyword;
    end else tokType := tkIdentifier;
  end;
  'T','t': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'TYPE') then begin
      tokType := tkKeyword;
    end else tokType := tkIdentifier;
  end;
  'U','u': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'UNTIL') then tokType := tkBlkDelim
    else tokType := tkIdentifier;
  end;
  'V','v': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'VAR') then tokType := tkKeyword
    else tokType := tkIdentifier;
  end;
  'X','x': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    //Can be optimized using a first verification by size of the string and not comparing the first letter.
    iden := Upcase(copy(curLine, col0, (fcol-col0)));
    if (iden = 'XOR') then begin
      tokType := tkOperator;
      tokPrec := 4;
    end else tokType := tkIdentifier;
  end;
  'F'..'H','J'..'L','Q'..'S','W','Y','Z','_',
  'f'..'h','j'..'l','q'..'s','w','y','z': begin
    repeat inc(fcol); until _Eol or not(curline[fcol] in ['_','a'..'z','A'..'Z','0'..'9']);
    tokType := tkIdentifier;
  end;
  '+','-': begin
    _NextChar;
    if not _Eol and (curLine[fcol]='=') then begin  //+=, -=
      _NextChar;
      tokType := tkOperator;
      tokPrec := 2;
    end else begin   //+, -
      tokType := tkOperator;
      tokPrec := 4;
      tokPrecU := 6;
    end;
  end;
  '~': begin
    _NextChar;
    tokType := tkOperator;
    tokPrec := 4;
    tokPrecU := 6;
  end;
  '*': begin
    _NextChar;
    if not _Eol and (curLine[fcol]='=') then begin  //*=
      _NextChar;
      tokType := tkOperator;
      tokPrec := 2;
    end else if not _Eol and (curLine[fcol]='*') then begin  //**
      _NextChar;
      tokType := tkOperator;
      tokPrec := 6;
    end else begin  //*
      tokType := tkOperator;
      tokPrec := 5;
    end;
  end;
  '/': begin
    _NextChar;
    if not _Eol and (curLine[fcol]='/') then begin  //Comment
      repeat _NextChar until _Eol;
      tokType := tkComment;
    end else if not _Eol and (curLine[fcol]='=') then begin // /=
      _NextChar;
      tokType := tkOperator;
      tokPrec := 2;
    end else begin  // /
      tokType := tkOperator;
      tokPrec := 5;
    end;
  end;
  '\': begin  //Not Pascal standard operators.
    _NextChar;
    tokType := tkOperator;
    tokPrec := 5;
  end;
  '.': begin  //Not Pascal standard operators.
    _NextChar;
    tokType := tkOperator;
    tokPrec := 6;
  end;
  '=': begin
    _NextChar;
    tokType := tkOperator;
    tokPrec := 3;
  end;
  '@','^': begin  //Special operators
    _NextChar;
    tokType := tkOperator;
    tokPrec := 6;
    tokPrecU := 6;
  end;
  '>': begin
    _NextChar;
    if not _Eol and (curLine[fcol] = '=') then begin  // >=
      _NextChar;
      tokType := tkOperator;
      tokPrec := 3;
    end else if not _Eol and (curLine[fcol] = '>') then begin  //SHR
      _NextChar;
      tokType := tkOperator;
      tokPrec := 5;
    end else begin  //>
      tokType := tkOperator;
      tokPrec := 3;
    end;
  end;
  '<': begin
    _NextChar;
    if not _Eol and (curLine[fcol] in ['=','>']) then begin  //<=, <>
      _NextChar;
      tokType := tkOperator;
      tokPrec := 3;
    end else if not _Eol and (curLine[fcol] = '<') then begin  //SHL
      _NextChar;
      tokType := tkOperator;
      tokPrec := 5;
    end else begin  //<
      tokType := tkOperator;
      tokPrec := 3;
    end;
  end;
  ';': begin
    _NextChar;
    tokType := tkExpDelim;
  end;
  ':': begin
    _NextChar;
    if _ReadChar = '=' then begin  //:=
      _NextChar;
      tokType := tkOperator;
      tokPrec := 2;
    end else begin  // :
      tokType := tkOthers;
    end;
  end;
  '(': begin
    _NextChar;
    if _ReadChar = '*' then begin
      _NextChar;
      //Multiline comment (* ... *)
      repeat _NextChar;
      until _Eof or (_Read2Char = '*)');
      if _Eof then begin
        onErrorScan('Unclosed comment.');  //Don't stop scanning
      end else begin
        _NextChar;  //Go to next character
        _NextChar;
      end;
      tokType := tkComment;
    end else begin
      tokType := tkOthers;
    end;
  end;
  ')',',','[',']': begin
    _NextChar;
    tokType := tkOthers;
  end;
  '''': begin
    repeat inc(fcol); until _Eol or (curline[fcol] = '''');
    if _Eol then begin
      onErrorScan('Unclosed string.');  //Don't stop scanning
    end else begin
      _NextChar;  //Go to next character
    end;
    tokType := tkString;
  end;
  '#': begin
    _NextChar;
    if _ReadChar = '$' then begin
      _NextChar;
      while not _Eol and (curline[fcol] in ['0'..'9','A'..'F','a'..'f']) do inc(fcol);
    end else  begin
      while not _Eol and (curline[fcol] in ['0'..'9']) do inc(fcol);
    end;
    tokType := tkChar;
  end;
  '{': begin
    _NextChar;
    if _ReadChar='$' then begin
      //Is directive
      repeat _NextChar;
      until _Eof or (_ReadChar = '}');
      if _Eof then begin
        onErrorScan('Unclosed directive.');  //Don't stop scanning
      end else begin
        _NextChar;  //Go to next character
      end;
      tokType := tkDirective;
    end else begin
      //Multiline comment
      repeat _NextChar;
      until _Eof or (_ReadChar = '}');
      if _Eof then begin
        onErrorScan('Unclosed comment.');  //Don't stop scanning
      end else begin
        _NextChar;  //Go to next character
      end;
      tokType := tkComment;
    end;
  end;
  else
    //Unkmown token.
    tokType := tkNull;
    _NextChar;
  end;
  exit(false);
end;
function TContext.Next: boolean;
{Decode the token in the current position of the cursor (frow,fcol) and returns:
- Start of the current token in (row0, col0).
- Token type in "tokType".
- Start of next token in (frow, fcol).
- Value TRUE if the current line has changed.
}
var
  tmp: String;
begin
  row0 := frow;
  col0 := fcol;
  if Empty then exit;
  if OnDecodeNext = nil then begin
    Result := DecodeNext;  //Decode token in (frow, fcol)
  end else begin
    //A hook has been introduced to the lexer
    Result := OnDecodeNext();  //Decode token in (frow, fcol)
  end;
end;
function TContext.ReadToken: string;
{Read the current token in "token", that is defined by (row0, col0) and (frow, fcol).}
var
  r: Integer;
  tmp: String;
  line: string;
begin
  if tokType in [tkNull, tkSpace, tkEol, tkComment] then begin
    //For optimization, returns empty in these token types.
    exit('');
  end;
  if row0 = frow then begin  //It's a single line token
    exit( copy(curLine, col0, (fcol-col0)) );
  end else begin //It's a multiline token
    line := curLines[row0-1];
    tmp := copy(line, col0, length(line) );
    for r:=row0+1 to frow-1 do begin
      tmp += LineEnding + curLines[r-1];
    end;
    line := curLines[frow-1];
    tmp += LineEnding + copy(line, 1, fcol);
    exit(tmp);
  end;
end;
function TContext.tokTypeStr: string;
{Returns tokType as string}
begin
  //Result := GetEnumName(TypeInfo(TTokenKind),Ord(tokType));
  WriteStr(Result, tokType)
end;
procedure TContext.GetContextState(out c: TContextState);
begin
  c.idCtx := idCtx;
  //Scanner attributes
  c.frow    := frow;
  c.fcol    := fcol;
  c.curLine := curLine;
  c.curSize := curSize;
  //Adittional attributes
  c.row0    := row0;
  c.col0    := col0;
  c.tokType := tokType;
  c.tokPrec := tokPrec;
  c.tokPrecU:= tokPrecU;
end;
procedure TContext.SetContextState(const c: TContextState);
begin
  //idCtx   := c.idCtx;  Wouldn't be needed.
  //Scanner attributes
  frow    := c.frow;
  fcol    := c.fcol;
  curLine := c.curLine;
  curSize := c.curSize;
  //Adittional attributes
  row0    := c.row0;
  col0    := c.col0;
  tokType := c.tokType;
  tokPrec := c.tokPrec;
  tokPrecU:= c.tokPrecU;
end;
procedure TContext.SaveContextState;
//Guarda el estado actual del lexer en la variable interna "fLexerState".
//Este estado incluye las coordenadas actuales de lectura en el Lexer.
begin
  GetContextState(fLexerState);
end;
procedure TContext.RestoreContextState;
//Copia el estado del lexer grabado en "fLexerState". Se debe ejecutar siempre
//después de SaveLexerState().
begin
  SetContextState(fLexerState);
end;
function TContext.ReadSource: string;
//Devuelve el contenido del contexto en una cadena.
begin
  Result := curLines.text;
end;
procedure TContext.StartScan;
{Reset the scanner to start working.}
begin
  nlin := curLines.Count; //Number of lines of source.
  //Set cursor
  setRow(1);
  setCol(1);
  row0 := 1;
  col0 := 1;
  //Updates the first token.
  Next;
end;
//Métodos de inicialización
procedure TContext.SetSource(txt: string);
//Fija el contenido del contexto con una cadena. Puede ser de varias líneas.
begin
  typ := TC_TXT;          //indica que contenido es Texto
  //guarda en lista interna.
  if txt='' then begin
    //cadena vacía, crea una línea vacía
    intLines.Clear;
  end else begin
    intLines.Text := txt;
  end;
  curLines := intLines;  //Apunta a almacenamiento interno
  fileSrc := '';        //There is not file source defined.
  StartScan;            //Updates first cursor position.
end;
procedure TContext.SetSource(lins: Tstrings; MakeCopy: boolean = false);
//Fija el contenido del contexto con una lista TStringList. Usa la referencia, no copia.
begin
  typ := TC_TXT;         //indica que contenido es Texto
  if MakeCopy then begin  //crea copia
    intLines.Clear;
    intLines.AddStrings(lins); //carga líneas, de la lista
    curLines := intLines; //apunta a almacenamiento interno
  end else begin
    curLines := lins;    //apunta a la fuente externa. No la copia.
  end;
  fileSrc := '';        //There is not file source defined.
  StartScan;            //Updates first cursor position.
end;
procedure TContext.SetSourceF(file0: string);
//Fija el contenido del contexto con un archivo
begin
  typ := TC_ARC;         //indica que contenido es Texto
  intLines.LoadFromFile(file0);
  curLines := intLines;  //apunta a almacenamiento interno
  fileSrc := file0;     //Takes file name.
  StartScan;            //Updates first cursor position.
end;
constructor TContext.Create;
begin
inherited;   //solo se pone por seguridad, ya que no es necesario.
  intLines := TStringList.Create;    //crea lista de cadenas para almacenar el texto
  nlin := 0;
  SetSource('');  //para iniciar con algo en donde leer
end;
destructor TContext.Destroy;
begin
//  lex.Free;     //libera lexer
  intLines.Free;     //libera lista
  inherited Destroy;
end;
{TAleLexer}
procedure TAleLexer.AleLexerErrorScan(txt: string);
{El lexer actual ha generado un error.
Este es el único caso en que TAleLexer genera un error.}
begin
  //Mandamos el mensaje de error al gestor.
  msg.error(GetMsgInfoE(txt));
end;
//Information for current context
function TAleLexer.GetCtxState: TContextState;
//Devuelve Contexto actual y su posición
begin
  if curCtx = nil then begin
    //Aún no hay Contexto definido
  end else begin
    curCtx.GetContextState(Result);
  end;
end;
procedure TAleLexer.SetCtxState(pc: TContextState);
//Fija Contexto actual y su posición
begin
  curCtx := ctxList[pc.idCtx];
  curCtx.SetContextState(pc);
  //Update current token and type
  token := curCtx.ReadToken;
  tokType := curCtx.tokType;
end;
function TAleLexer.GetSrcPos: TSrcPos;
{Devuelve un objeto TSrcPos, en la posición actual.}
begin
  if curCtx = nil then begin
    { #todo : Validar si es equivalente usar "srcPosNull" en este caso.}
//    Result.row := -1;
//    Result.col := -1;
//    Result.idCtx := 0;
    Result := srcPosNull;
  end else begin
    Result.idCtx := curCtx.idCtx;
    Result.Row := curCtx.row0;
    Result.Col := curCtx.col0;
  end;
end;
procedure TAleLexer.SetSrcPos(const srcPos: TSrcPos);
begin
  curCtx := ctxList[srcPos.idCtx];
  if curCtx = nil then begin
    //No tiene un Contexto actual
  end else begin
    curCtx.row0 := srcPos.row;
    curCtx.col0 := srcPos.col;
  end;
end;
function TAleLexer.GetMsgInfo(const txt: string): TMsgInfo;
{Devuelve un objeto de mensaje (TMsgInfo) con el texto "txt" y la posición actual en el
código fuente}
begin
  if curCtx = nil then begin
    Result.txt := txt;
    Result.fname := '';
    Result.row := -1;
    Result.col := -1;
  end else begin
    //Devuelve información del contexto actual
    Result.txt := txt;
    Result.fname := curCtx.fileSrc;
    Result.Row := curCtx.row0;
    Result.Col := curCtx.col0;
  end;
end;
function TAleLexer.GetMsgInfoE(const txt: string): TMsgInfo;
{Devuelve un objeto de mensaje (TMsgInfo) con el texto "txt" y la posición actual en el
código fuente. Detecta la condición "FixErrPos"}
begin
  if curCtx = nil then begin
    Result.txt := txt;
    Result.fname := '';
    Result.row := -1;
    Result.col := -1;
  end else begin
    if curCtx.FixErrPos then begin
      //El contexto actual, tiene configurado una posición fija para los errores
      Result.txt := curCtx.PreErrorMsg + txt;  //completa mensaje
      Result.fname := ctxFile(curCtx.PreErrPosit.idCtx);
      Result.Row := curCtx.PreErrPosit.row;
      Result.Col := curCtx.PreErrPosit.col;
    end else begin
      Result.txt := txt;
      Result.fname := curCtx.fileSrc;
      Result.Row := curCtx.row0;
      Result.Col := curCtx.col0;
    end;
  end;
end;
function TAleLexer.GetMsgInfo(txt: string; const srcPos: TSrcPos): TMsgInfo;
{Devuelve un objeto de mensaje (TMsgInfo) con el texto "txt" y la posición "srcPos"}
begin
  //Devuelve información del contexto actual
  Result.txt := txt;
  if srcPos.idCtx = -1 then  //Posición nula
    Result.fname := ''
  else
    Result.fname := ctxFile(srcPos.idCtx);
  Result.Row := srcPos.row;
  Result.Col := srcPos.col;
end;
function TAleLexer.GetMsgInfoE(const txt: string; const srcPos: TSrcPos): TMsgInfo;
{Devuelve un objeto de mensaje (TMsgInfo) con el texto "txt" y la posición "srcPos".
Detecta la condición "FixErrPos"}
begin
  if (curCtx <> nil) and curCtx.FixErrPos then begin
    //El contexto actual, tiene configurado una posición fija para los errores
    Result.txt := curCtx.PreErrorMsg + txt;  //completa mensaje
    Result.fname := ctxFile(curCtx.PreErrPosit.idCtx);
    Result.Row := curCtx.PreErrPosit.row;
    Result.Col := curCtx.PreErrPosit.col;
  end else begin
    //Caso normal
    Result.txt := txt;
    if srcPos.idCtx = -1 then  //Posición nula
      Result.fname := ''
    else
      Result.fname := ctxFile(srcPos.idCtx);
    Result.Row := srcPos.row;
    Result.Col := srcPos.col;
  end;
end;
//Information for any context
function TAleLexer.ctxId(fileSrc: string): integer;
{Returns the ID of a context whose fileSrc attribute is the indicated.}
var
  i: Integer;
begin
  fileSrc := UpCase(fileSrc);
  for i:=0 to ctxList.Count-1 do begin
    if UpCase(ctxList[i].fileSrc) = fileSrc then exit(i);
  end;
  //Not found
  exit(-1);
end;
function TAleLexer.ctxFile(idCtx: integer): string;
{Returns the file name for some context.}
var
  ct: TContext;
begin
  if idCtx<0 then exit('');
  ct := ctxList[idCtx];
  exit(ct.fileSrc);
end;
function TAleLexer.ctxFile(const srcPos: TSrcPos): string;
{Returns the file name for some context, receiving a TSrcPos.}
var
  ct: TContext;
begin
  if srcPos.idCtx<0 then exit('');
  ct := ctxList[srcPos.idCtx];
  exit(ct.fileSrc);
end;
function TAleLexer.ctxFileName(const srcPos: TSrcPos): string;
{Returns the file name (like file1.pas ) from the source file of a context}
var
  ct: TContext;
begin
  if srcPos.idCtx<0 then exit('');
  ct := ctxList[srcPos.idCtx];
  exit(ExtractFileName(ct.fileSrc));
end;
function TAleLexer.ctxFileDir(const srcPos: TSrcPos): string;
{Returns the file directory (like C:\dir1 ) from the source file of a context}
var
  ct: TContext;
begin
  if srcPos.idCtx<0 then exit('');
  ct := ctxList[srcPos.idCtx];
  exit(ExtractFileDir(ct.fileSrc));
end;
//Context manage
function TAleLexer.AddContext: TContext;
{Add a context to "ctxList" and returns the reference.
Punto único para agregar un contexto}
begin
  inherited;
  Result := TContext.Create;   //Creates Context.
  Result.retPos := GetCtxState; //Keep return position.
  Result.onErrorScan := @AleLexerErrorScan;
  ctxList.Add(Result);         //Register Context.
  idCount := ctxList.Count-1;  //Calculate the index.
  Result.idCtx := idCount;     //Set reference to index.
end;
procedure TAleLexer.NewContextFromText(txt: string; fileSrc: String);
{Create a new context from a text and set "curCtx" to the new context created.
Parameter "filSrc" is a optional reference to a file name, asociated to "txt".}
begin
  curCtx := AddContext;
  {$ifdef debug_mode}
  debugln('  +Nex context from Txt:'+filSrc);
  {$endif}
  curCtx.SetSource(txt);      //Inicia con texto
  curCtx.fileSrc := fileSrc;  {Se guarda el nombre del archivo actual, solo para poder procesar
                               las funciones $NOM_ACTUAL y $DIR_ACTUAL}
  //Actualiza token actual
  token := curCtx.ReadToken;  //lee el token
  tokType := curCtx.tokType;  //lee atributo
end;
procedure TAleLexer.NewContextFromFile(filSrc: String; out notFound: boolean);
{Create a new context from a file and set "curCtx" to the new context created.
If the file is not found, returns TRUE in "notFound".}
begin
  notFound := false;
  If not FileExists(filSrc)  Then  begin  //ve si existe
    notFound := true;
    exit;
  end;
  curCtx := AddContext;
  {$ifdef debug_mode}
  debugln('  +Nex context from File:'+filSrc);
  {$endif}
  curCtx.SetSourceF(filSrc);   //Inicia con archivo
  //Actualiza token actual
  token := curCtx.ReadToken;    //lee el token
  tokType := curCtx.tokType;  //lee atributo
end;
procedure TAleLexer.NewContextFromTStrings(lins: Tstrings; filSrc: String);
{Create a new context from a TStrings and set "curCtx" to the new context created.
Parameter "filSrc" is a optional reference to a file name, asociated to "lins".}
begin
  curCtx := AddContext;
  {$ifdef debug_mode}
  debugln('  +Nex context from File:'+filSrc);
  {$endif}
  curCtx.SetSource(lins);     //Inicia con archivo contenido en TStrings
  curCtx.fileSrc :=  filSrc;  //Guarda nombre de archivo, solo como referencia.
  //actualiza token actual
  token := curCtx.ReadToken;  //lee el token
  tokType := curCtx.tokType;  //lee atributo
end;
procedure TAleLexer.ReturnToPrevContext(remove: boolean);
{Set the current context to the previous context. The previous context is the context
that active when the current context was opened. After return to the previous context
the state es restored too. Is "remove" is set, the current context is deleted from
"ctxList".}
var
  retPos: TContextState;
begin
  if ctxList.Count = 0 then begin
    //No hay contextos abiertos
    curCtx := nil;   //por si acaso
    exit;  //no se puede quitar más
  end;
  {$ifdef debug_mode}
  debugln('  -Context deleted:'+ curCtx.arc);
  {$endif}
  //Hay al menos un contexto abierto
  retPos := curCtx.retPos;  //guarda dirección de retorno
  //ctxList.Delete(ctxList.Count-1);  //elimina contexto superior
  if remove then ctxList.Remove(curCtx);
  if ctxList.Count = 0 then begin
    //No quedan contextos abiertos
    curCtx := nil;
  end else begin
    //Queda al menos un contexto anterior
    curCtx := ctxList[retPos.idCtx]; //Recover last context
    SetCtxState(retPos);             //Recover last position
  end;
end;
procedure TAleLexer.ClearContexts;  //Limpia todos los contextos
begin
  ctxList.Clear;  //Elimina todos los Contextos de entrada
  curCtx := nil;    //Por si acaso
  idCount := 0;   //Inicia contador
end;

function TAleLexer.OpenContextFrom(filePath: string): boolean;
{Abre un contexto con el archivo indicado. Si lo logra abrir, devuelve TRUE.}
var
  strList: TStrings;
  notFound: boolean;
begin
  //Primero ve si puede obteenr acceso directo al contenido del archivo
  if OnRequireFileString<>nil then begin
    //Hace la consulta a través del evento
    strList := nil;
    OnRequireFileString(filePath, strList);
    if strList=nil then begin
      //No hay acceso directo al contenido. Carga de disco
      //debugln('>disco:'+filePath);
      NewContextFromFile(filePath, notFound);
      Result := not notFound;  //El único error es cuando no se encuentra el archivo.
    end else begin
      //Nos están dando el acceso al contenido. Usamos "strList"
      NewContextFromTStrings(strList, filePath);
      Result := true;
    end;
  end else begin
    //No se ha establecido el evento. Carga de disco
    //debugln('>disco:'+filePath);
    NewContextFromFile(filePath, notFound);
    Result := not notFound;  //El único error es cuando no se encuentra el archivo.
  end;
end;

//Scan functions
function TAleLexer.tokL: string;
//Devuelve el token actual, ignorando la caja.
begin
  exit(lowercase(token));
end;
function TAleLexer.atEol: Boolean;
begin
  exit(tokType = tkEol);
  {Note "curCtx.Eol" won't work correctly, because it respond to "fcol", not
  to "col0".}
end;
function TAleLexer.atEof: Boolean;
begin
  //A complete verificaction must consider: if nlin = 0 then ..
  exit((curCtx.row0 = curCtx.nlin) and  //Last line
       (curCtx.col0 = curCtx.curSize));  //At EOL
  {Note "curCtx.Eof" won't work correctl, because it respond to (frow, fcol), no to
  (row0, col0).}
end;
procedure TAleLexer.SkipWhites;
{Salta los blancos incluidos los saltos de línea}
begin
  while atEof or  //Considera también, para poder auto-cerrar contextos
       (curCtx.tokType in [tkSpace, tkEol, tkComment]) do
  begin
      if atEof then begin
        if curCtx.autoReturn then begin
          ReturnToPrevContext(curCtx.autoRemove);  //Retorna al contexto padre.
        end else begin
          break;  //Sale del WHILE
        end;
      end;
      if curCtx.Next then begin   //hubo cambio de línea
        if OnNewLine<>nil then OnNewLine(curCtx.CurLine);
      end;
  end;
  //Actualiza token actual
  token := curCtx.ReadToken;    //lee el token
  tokType := curCtx.tokType;  //lee atributo
end;
procedure TAleLexer.SkipWhitesNoEOL;
{Salta los blancos sin incluir los saltos de línea}
begin
  while not atEof and (curCtx.tokType in [tkSpace, tkComment]) do
  begin
      if curCtx.Next then begin   //hubo cambio de línea
        if OnNewLine<>nil then OnNewLine(curCtx.CurLine);
      end;
  end;
  //actualiza token actual
  token := curCtx.ReadToken;    //lee el token
  tokType := curCtx.tokType;  //lee atributo
end;
procedure TAleLexer.Next;
begin
  if curCtx.Next then begin   //hubo cambio de línea
    if OnNewLine<>nil then OnNewLine(curCtx.CurLine);
  end;
  if atEof then begin
    if curCtx.autoReturn then begin
      ReturnToPrevContext(curCtx.autoRemove);  //Retorna al contexto padre.
    end;
  end;
  //actualiza token actual
  token := curCtx.ReadToken;    //lee el token
  tokType := curCtx.tokType;  //lee atributo
end;
procedure TAleLexer.GotoEOL;
{Move lexer cursor to the end of the current line.}
begin
  curCtx.setEOL;   //Move fcol to end of the line.
  curCtx.Next;     //Update current token to tkEOL
end;
procedure TAleLexer.ShowContexts;
{Función para depuración. Muestra el contenido de los contextos existentes.}
var ct: TContext;
begin
  debugln('=== Opened contexts ===');
  for ct in ctxList do begin
    debugln('idCtx=' + IntToStr(ct.idCtx) + '   ' + ct.fileSrc);
  end;
end;
procedure TAleLexer.ShowCurContInformat;
var
  typStr: string;
begin
  case curCtx.typ of
  TC_ARC: typStr := 'TC_ARC';
  TC_TXT: typStr := 'TC_TXT';
  end;
  debugln('===Current Context ===');
  debugln('  idCtx=' + IntToStr(curCtx.idCtx));
  debugln('  arc=' + curCtx.fileSrc);
  debugln('  typ=%s pos=[%d,%d]', [typStr, curCtx.row, curCtx.col]);
//  debugln('  curlines=' + curCon.curLines.Text);
end;
//Initialization
constructor TAleLexer.Create(msg0: TMessageManager);
begin
  msg := msg0;
  ctxList := TContextList.Create(true);  //crea contenedor de Contextos, con control de objetos.
  curCtx := nil;
end;
destructor TAleLexer.Destroy;
begin
  ctxList.Free;
  inherited Destroy;
end;

Initialization
  srcPosNull.row := -1;
  srcPosNull.col := -1;
  srcPosNull.idCtx := -1;
//srcPos.idCtx := 0;


end.

