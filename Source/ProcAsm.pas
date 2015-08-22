{Unidad para procesar código ensamblador del PIC}
unit ProcAsm;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, strutils, MisUtils, SynFacilHighlighter,
  SynEditHighlighter;
var
  asmRow: integer;   //número de fila explorada

procedure ProcASMlime(const AsmLin: string);  //Procesa una línea de código ASM

implementation
var
  InAsm : boolean;
  lexAsm : TSynFacilSyn;  //lexer para analizar ASM

function tokType: TSynHighlighterAttributes;
begin
  Result := TSynHighlighterAttributes(lexAsm.GetTokenKind);
end;
procedure skipWhites;
begin
if tokType = lexAsm.tkSpace then
  lexAsm.Next;  //quita espacios
end;
procedure StartASM; //Inicia el procesamiento de código ASM
begin
  InAsm := true;  //para indicar que estamos en medio de código ASM
  asmRow := 1;    //inicia línea
end;
procedure EndASM;  //Termina el procesamiento de código ASM
begin
  InAsm := false;
end;
procedure ProcASM(const AsmLin: string);
{Procesa una línea en ensamblador}
begin
  inc(asmRow);   //cuenta líneas
  if Trim(AsmLin) = '' then exit;
  //procesa la línea
  lexAsm.SetLine(asmLin, asmRow);  //inicia cadena
  if tokType = lexAsm.tkKeyword then begin
    //puede ser una instrucción

  end;
  skipWhites;  //quita espacios

  msgbox(AsmLin);
end;
function IsStartASM(var lin: string): boolean;
{Indica si una línea contiene al delimitador inicial "ASM". Si es así, la recorta.}
begin
  if not AnsiStartsText('asm', lin) then
    exit(false);  //definitivamente no es
  //hay coincidencia pero hay que analziar más
  if length(lin) = 3 then begin
    lin := copy(lin, 4, length(lin));  //quita "asm"
    exit(true);  //es exacto
  end;
  //podrìa ser, pero hay que descartar que no sea parte de un identificador
  if lin[4] in ['a'..'z', 'A'..'Z', '0'..'9', '_'] THEN
    exit(false); //es parte de un identificador
  //es por descarte
  lin := copy(lin, 4, length(lin));  //quita "asm"
  exit(true);
end;
function IsEndASM(var  lin: string): boolean;
{Indica si una línea contiene al delimitador final "END". Si es así, la recorta.}
begin
  if not AnsiEndsText('end', lin) then
    exit(false);  //definitivamente no es
  //hay coincidencia pero hay que analziar más
  if length(lin) = 3 then begin
    lin := copy(Lin, 1, length(Lin)-3);  //quita "end"
    exit(true);  //es exacto
  end;
  //podrìa ser, pero hay que descartar que no sea parte de un identificador
  if lin[length(lin)-3] in ['a'..'z', 'A'..'Z', '0'..'9', '_'] THEN
    exit(false); //es parte de un identificador
  //es por descarte
  lin := copy(Lin, 1, length(Lin)-3);  //quita "end"
  exit(true);
end;
procedure ProcASMlime(const AsmLin: string); //Procesa una línea de código ASM
var
  lin: String;
begin
  lin := AsmLin;  //crea copia para poder modificarla
  //Extrae el texto entre los delimitadores de ensamblador
  if IsStartASM(lin) then begin
    //es la primera línea de ensamblador
    StartASM;
    //puede incluir también al delimitador "end"
    if IsEndASM(lin) then begin
      ProcASM(lin);  //procesa por si queda código
      EndASM;
    end else begin
      ProcASM(lin);  //procesa por si queda código
    end;
  end else if IsEndASM(lin) then begin
    //es la última línea de ensamblador
    ProcASM(lin);  //procesa por si queda código
    EndASM;
  end else begin
    //línea común de asm
    ProcASM(lin);
  end;
end;

initialization
  InAsm := false;
  {Define la sintaxis del lexer que se usará para analizar el código en ensamblador.}
  lexAsm := TSynFacilSyn.Create(nil);  //crea lexer para analzar ensamblador
  lexAsm.DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');
  lexAsm.DefTokContent('[0-9]', '[0-9.]*', lexAsm.tkNumber);
  lexAsm.DefTokContent('[$]','[0-9A-Fa-f]*', lexAsm.tkNumber);
  lexAsm.DefTokContent('[%]','[01]*', lexAsm.tkNumber);
  lexAsm.AddIdentSpecList('ADDWF ANDWF CLRF CLRW COMF DECF DECFSZ INCF', lexAsm.tkKeyword);
  lexAsm.AddIdentSpecList('INCFSZ IORWF MOVF MOVWF NOP RLF RRF SUBWF SWAPF XORWF', lexAsm.tkKeyword);
  lexAsm.AddIdentSpecList('BCF BSF BTFSC BTFSS', lexAsm.tkKeyword);
  lexAsm.AddIdentSpecList('ADDLW ANDLW CALL CLRWDT GOTO IORLW MOVLW RETFIE', lexAsm.tkKeyword);
  lexAsm.AddIdentSpecList('RETLW RETURN SLEEP SUBLW XORLW', lexAsm.tkKeyword);
  lexAsm.DefTokDelim(';','', lexAsm.tkComment);
  lexAsm.Rebuild;
finalization
  lexAsm.Destroy;
end.

