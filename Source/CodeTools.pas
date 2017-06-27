{Unidad con funciones de exploración de código}
unit CodeTools;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LCLType, SynEdit, SynEditHighlighter, MisUtils,
  SynFacilCompletion, SynFacilHighlighter, XpresBas, XpresElementsPIC,
  FrameEditView, FrameSyntaxTree, Parser;

type

  { TCodeTool }

  TCodeTool = class
  private
    //Referencias importantes
    fraEdit   : TfraEditView;
    cxp       : TCompiler;
    fraSynTree: TfraSyntaxTree;
  public
    procedure ReadCurIdentif(out tok: string; out tokType: integer; out
      lex: TSynFacilComplet; out curX: integer);
    procedure GoToDeclaration;
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    constructor Create(fraEdit0: TfraEditView; cxp0: TCompiler;
      fraSynTree0: TfraSyntaxTree);
  end;

implementation

procedure TCodeTool.ReadCurIdentif(out tok: string; out tokType: integer;
                                   out lex: TSynFacilComplet; out curX: integer);
{DA infomación sobre el token actual. Si no encuentar información, devuelve cadena
nula en "tok".}
var
  sed: TSynEdit;
  lin: String;
  toks: TATokInfo;
  hl: TSynCustomHighlighter;
  tokIdx: integer;
begin
  if fraEdit.Count=0 then begin
    tok := '';
    exit;
  end;
  sed := fraEdit.ActiveEditor.SynEdit;
  if sed.Lines.Count = 0 then begin
    tok := '';
    exit;
  end;
  hl := sed.Highlighter;  //toma su resalatdor
  if hl is TSynFacilComplet then begin
    //Es TSynFacilComplet, usamos su propio resaltador como lexer
    //Además el mismo resaltador tiene acceso al contenido del SynEdit
    lex := TSynFacilComplet(hl);  //accede a TSynFacilComplet
    lex.ExploreLine(sed.CaretXY, toks, tokIdx);  //Explora línea actual
    tok := toks[tokIdx].txt;
    curX := toks[tokIdx].posIni+1;
    tokType := toks[tokIdx].TokTyp;
//    MsgBox('%d', [high(toks)]);
  end else begin
    //Es otro resaltador
    lin := sed.Lines[sed.CaretY - 1];
    tok := '';
  end;
end;
procedure TCodeTool.GoToDeclaration;
{Salta a la zona de declaración, del elemento que está bajo el cursor, en al ventana de
edición actual. Solo salta, si logra identificar al identificador.}
var
  tok: string;
  tokType, curX: integer;
  lex: TSynFacilComplet;
  callPos: TSrcPos;
  ed: TSynEditor;
  ele: TxpElement;
  curBody: TxpEleBody;
begin
  ed := fraEdit.ActiveEditor;
  //Primero ubica el token
  ReadCurIdentif(tok, tokType, lex, curX);
  if tok='' then exit;  //No encontró token
  if tokType <> lex.tnIdentif then exit;  //No es identificador
  //Asegurarse que "synTree" está actualizado.
  cxp.Compile(fraEdit.ActiveEditor.NomArc, false);  //Solo primera pasada
  if cxp.HayError then begin
    MsgErr('Compilation error.');  //tal vez debería dar más información sobre el error
    exit;
  end;
  callPos.col := curX;
  callPos.row := ed.SynEdit.CaretY;
  callPos.fil := ed.NomArc;
  ele := cxp.TreeElems.GetElementCalledAt(callPos);
  if ele = nil then begin
    //No lo ubica, puede ser que esté en la sección de declaración
    curBody := cxp.TreeElems.GetElementBodyAt(ed.SynEdit.CaretXY);
    if curBody=nil then begin


    end;
    MsgExc('Unknown identifier: %s', [tok]);
  end else begin
//      MsgBox('%s', [ele.name]);
    //Ubica la declaración del elemento
    if not fraEdit.SelectOrLoad(ele.srcDec, false) then begin
      MsgExc('Cannot load file: %s', [ele.srcDec.fil]);
    end;
  end;
end;
procedure TCodeTool.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{Procesa el evento de teclado}
begin
  if not fraEdit.HasFocus then exit;
  if fraEdit.Count=0 then exit;
  if (Shift = [ssAlt]) and (Key = VK_UP) then begin
    //Se pide ubicar la declaración del elemento
    GoToDeclaration;
  end;
end;
constructor TCodeTool.Create(fraEdit0: TfraEditView; cxp0 : TCompiler;
                             fraSynTree0: TfraSyntaxTree);
begin
  fraEdit    := fraEdit0;
  cxp        := cxp0;
  fraSynTree := fraSynTree0;
end;

end.

