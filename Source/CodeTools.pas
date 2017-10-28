{Unidad con funciones de exploración de código}
unit CodeTools;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LCLType, SynEdit, SynEditHighlighter, MisUtils,
  SynFacilCompletion, SynFacilHighlighter, XpresBas, XpresElementsPIC,
  FrameEditView, FrameSyntaxTree, Parser, XpresParserPIC;

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
    procedure FieldsComplet(ident: string; opEve: TFaOpenEvent; tokPos: TSrcPos);
    constructor Create(fraEdit0: TfraEditView; cxp0: TCompiler;
      fraSynTree0: TfraSyntaxTree);
  end;

implementation

procedure TCodeTool.ReadCurIdentif(out tok: string; out tokType: integer;
                                   out lex: TSynFacilComplet; out curX: integer);
{Da infomación sobre el token actual. Si no encuentra información, devuelve cadena
nula en "tok".}
var
  sed: TSynEdit;
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
//    lin := sed.Lines[sed.CaretY - 1];
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
//  curBody: TxpEleBody;
begin
  ed := fraEdit.ActiveEditor;
  //Primero ubica el token
  ReadCurIdentif(tok, tokType, lex, curX);
  if tok='' then exit;  //No encontró token
  if tokType <> lex.tnIdentif then exit;  //No es identificador
  //Asegurarse que "synTree" está actualizado.
  cxp.Compile(fraEdit.ActiveEditor.NomArc, false);  //Solo primera pasada
  if cxp.HayError then begin
    //Basta que haya compilado hasta donde se encuentra el identifiacdor, para que funciones.
//    MsgErr('Compilation error.');  //tal vez debería dar más información sobre el error
//    exit;
  end;
  callPos.col := curX;
  callPos.row := ed.SynEdit.CaretY;
  callPos.fil := ed.NomArc;
  ele := cxp.TreeElems.GetElementCalledAt(callPos);
  if ele = nil then begin
    //No lo ubica, puede ser que esté en la sección de declaración
    ele := cxp.TreeElems.GetELementDeclaredAt(callPos);
    if ele <> nil then begin
      //Es el punto donde se declara
      if ele.idClass = eltUnit then begin
        fraEdit.SelectOrLoad(TxpEleUnit(ele).srcFile);
//        MsgBox(ele.name);
      end else begin
        //Es otra declaración
      end;
    end else begin
      MsgExc('Unknown identifier: %s', [tok]);
    end;
//    curBody := cxp.TreeElems.GetElementBodyAt(ed.SynEdit.CaretXY);
//    if curBody=nil then begin
//
//    end;
  end else begin
//      MsgBox('%s', [ele.name]);
    //Ubica la declaración del elemento
    if not fraEdit.SelectOrLoad(ele.srcDec, false) then begin
      MsgExc('Cannot load file: %s', [ele.srcDec.fil]);
    end;
  end;
end;
procedure TCodeTool.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{Procesa el evento de teclado, para cuando se tiene el editor seleccionado.}
var
  ed: TSynEditor;
begin
  if not fraEdit.HasFocus then exit;
  if fraEdit.Count=0 then exit;
  ed := fraEdit.ActiveEditor;
  if (Shift = [ssAlt]) and (Key = VK_UP) then begin
    //Se pide ubicar la declaración del elemento
    GoToDeclaration;
  end;
  if not ed.SynEdit.SelAvail then begin
    //No hay selección. Pero se pulsa ...
    if (Shift = [ssCtrl]) and (Key = VK_C) then begin  //Ctrl+C
      ed.SynEdit.SelectWord;
      ed.Copy;
    end;
    if (Shift = [ssCtrl]) and (Key = VK_INSERT) then begin  //Ctrl+Insert
      ed.SynEdit.SelectWord;
      ed.Copy;
    end;
  end;
end;
//Opciones para completado de código
procedure TCodeTool.FieldsComplet(ident: string; opEve: TFaOpenEvent;
  tokPos: TSrcPos);
{LLena un objeto opEve, con los campos del identificador "ident", de acuerdo a su tipo.}
{Se solicita lista de campos de un identificador, para el completado de código en el
editor.}
var
  ele: TxpElement;
  xVar: TxpEleVar;
begin
  opEve.ClearItems;  //limpia primero
  //Asegurarse que "synTree" está actualizado.
  cxp.Compile(fraEdit.ActiveEditor.NomArc, false);  //Solo primera pasada
  if cxp.HayError then begin
    //Basta que haya compilado hasta donde se encuentra el identificador, para que funciones.
//    MsgErr('Compilation error.');  //tal vez debería dar más información sobre el error
//    exit;
  end;
  ele := cxp.TreeElems.GetElementCalledAt(tokPos);
  if ele = nil then begin
    //No identifica a este elemento
    exit;
  end;
  if ele.idClass = eltVar then begin
    //Es una variable, vemos el tipo
    xVar := TxpEleVar(ele);
    if xVar.typ = typByte then begin
      opEve.AddItem('bit0', 11);
      opEve.AddItem('bit1', 11);
      opEve.AddItem('bit2', 11);
      opEve.AddItem('bit3', 11);
      opEve.AddItem('bit4', 11);
      opEve.AddItem('bit5', 11);
      opEve.AddItem('bit6', 11);
      opEve.AddItem('bit7', 11);
    end;
    if xVar.typ = typWord then begin
      opEve.AddItem('high', 11);
      opEve.AddItem('low' , 11);
    end;
  end else begin
    //No implementado en otro elemento
    exit;
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

