{                                   PicPas.
Compilador en Pascal para micorocntroladores PIC de la serie 16.

                                        Por Tito Hinostroza   22/08/2015 }
unit FormPrincipal;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, SynEdit, SynEditTypes, Forms, Controls, Dialogs, Menus,
  ComCtrls, ActnList, StdActns, ExtCtrls, LCLIntf, LCLType, LCLProc,
  SynFacilHighlighter, SynFacilUtils, MisUtils, XpresBas,
  Pic16Utils, Parser, FormPICExplorer, Globales, FrameSyntaxTree, FormConfig,
  PicPasProject, FrameEditView, FrameMessagesWin, XpresElementsPIC, CodeTools,
  ParserAsm, ParserDirec, FrameCfgExtTool, FormDebugger, FormRAMExplorer;
type
  { TfrmPrincipal }
  TfrmPrincipal = class(TForm)
    acArcOpen: TAction;
    acArcSaveAs: TAction;
    acArcSave: TAction;
    acArcNewFile: TAction;
    acArcQuit: TAction;
    acSearFind: TAction;
    acSearFindNxt: TAction;
    acSearReplac: TAction;
    acEdCopy: TEditCopy;
    acEdCut: TEditCut;
    acEdPaste: TEditPaste;
    acEdRedo: TAction;
    acEdSelecAll: TAction;
    acEdUndo: TAction;
    acArcNewProj: TAction;
    acArcCloseProj: TAction;
    acArcCloseFile: TAction;
    acSearFindPrv: TAction;
    acToolASMDebug: TAction;
    acViewAsmPan: TAction;
    acToolRamExp: TAction;
    acToolExt4: TAction;
    acToolExt5: TAction;
    acToolExt2: TAction;
    acToolExt3: TAction;
    acToolExt1: TAction;
    acToolFindDec: TAction;
    acToolListRep: TAction;
    acToolConfig: TAction;
    acToolCompil: TAction;
    acToolPICExpl: TAction;
    acToolComEjec: TAction;
    acViewToolbar: TAction;
    acViewMsgPan: TAction;
    ActionList: TActionList;
    acViewStatbar: TAction;
    acViewSynTree: TAction;
    edAsm: TSynEdit;
    FindDialog1: TFindDialog;
    ImgActions32: TImageList;
    ImgActions16: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem8: TMenuItem;
    mnSamples: TMenuItem;
    mnView: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    mnExit: TMenuItem;
    MenuItem9: TMenuItem;
    mnFile: TMenuItem;
    mnFind: TMenuItem;
    mnEdit: TMenuItem;
    mnTools: TMenuItem;
    mnRecents: TMenuItem;
    panMessages: TPanel;
    PopupEdit: TPopupMenu;
    ReplaceDialog1: TReplaceDialog;
    splSynTree: TSplitter;
    Splitter2: TSplitter;
    splEdPas: TSplitter;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure acArcCloseFileExecute(Sender: TObject);
    procedure acArcCloseProjExecute(Sender: TObject);
    procedure acArcOpenExecute(Sender: TObject);
    procedure acArcSaveAsExecute(Sender: TObject);
    procedure acArcSaveExecute(Sender: TObject);
    procedure acArcNewFileExecute(Sender: TObject);
    procedure acArcNewProjExecute(Sender: TObject);
    procedure acArcQuitExecute(Sender: TObject);
    procedure acSearFindExecute(Sender: TObject);
    procedure acSearFindNxtExecute(Sender: TObject);
    procedure acSearFindPrvExecute(Sender: TObject);
    procedure acSearReplacExecute(Sender: TObject);
    procedure acEdiRedoExecute(Sender: TObject);
    procedure acEdiSelecAllExecute(Sender: TObject);
    procedure acEdiUndoExecute(Sender: TObject);
    procedure acEdRedoExecute(Sender: TObject);
    procedure acEdSelecAllExecute(Sender: TObject);
    procedure acEdUndoExecute(Sender: TObject);
    procedure acToolCompilExecute(Sender: TObject);
    procedure acToolASMDebugExecute(Sender: TObject);
    procedure acToolListRepExecute(Sender: TObject);
    procedure acToolConfigExecute(Sender: TObject);
    procedure acToolExt1Execute(Sender: TObject);
    procedure acToolExt2Execute(Sender: TObject);
    procedure acToolExt3Execute(Sender: TObject);
    procedure acToolExt4Execute(Sender: TObject);
    procedure acToolExt5Execute(Sender: TObject);
    procedure acToolFindDecExecute(Sender: TObject);
    procedure acToolPICExplExecute(Sender: TObject);
    procedure acToolRamExpExecute(Sender: TObject);
    procedure acViewAsmPanExecute(Sender: TObject);
    procedure acViewSynTreeExecute(Sender: TObject);
    procedure acViewStatbarExecute(Sender: TObject);
    procedure acViewToolbarExecute(Sender: TObject);
    procedure acViewMsgPanExecute(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure fraEdit_ChangeEditorState(ed: TSynEditor);
    procedure DoSelectSample(Sender: TObject);
    procedure editChangeFileInform;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ReplaceDialog1Replace(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    tic         : integer;  //Contador para temporización
    ticSynCheck : integer;  //Contador para temporizar la verifiación ed sintaxis
    curProj     : TPicPasProject; //Proyecto actual
    hlAssem     : TSynFacilSyn;   //resaltador para ensamblador
    fraEditView1: TfraEditView;   //Panel de editores
    fraSynTree  : TfraSyntaxTree; //Árbol de sintaxis
    fraMessages : TfraMessagesWin;
    CT          : TCodeTool;
    procedure ConfigExtTool_RequirePar(var comLine: string);
    procedure cxp_AfterCompile;
    procedure cxp_RequireFileString(FilePath: string; var strList: TStrings);
    procedure fraEdit_RequireSetCompletion(ed: TSynEditor);
    procedure fraMessagesStatisDBlClick;
    procedure fraSynTreeSelecFileExplorer;
    procedure fraEdit_RequireSynEditConfig(ed: TsynEdit);
    procedure ChangeAppearance;
    procedure fraEdit_SelectEditor;
    procedure fraMessagesDblClickMessage(const srcPos: TSrcPos);
    procedure fraSynTreeOpenFile(filname: string);
    procedure fraSynTreeSelectElemen(var elem: TxpElement);
    procedure LoadAsmSyntaxEd;
    procedure MarcarError(ed: TSynEditor; nLin, nCol: integer);
    procedure MarkErrors;
    procedure VerificarError;
  public
    frmDebug: TfrmDebugger;
    procedure SetLanguage(idLang: string);
  end;

var
  frmPrincipal: TfrmPrincipal;
var
  MSG_MODIF   : string;
  MSG_SAVED   : string;
  MSG_NOFILES : string;
  MSG_NOFOUND_: string;
  MSG_REPTHIS : string;
  MSG_N_REPLAC: string;
implementation
{$R *.lfm}
{ TfrmPrincipal }
procedure TfrmPrincipal.SetLanguage(idLang: string);
begin
  if curLanguage = idLang then
    exit;  //no ha habido cambio de idioma
  curLanguage := idLang;
  Config.SetLanguage;
  fraSynTree.SetLanguage;
  fraEditView1.SetLanguage;
  fraMessages.SetLanguage;
  Parser.SetLanguage;
  ParserAsm.SetLanguage;
  ParserDirec.SetLanguage;
  {$I ..\language\tra_FormPrincipal.pas}
end;
procedure TfrmPrincipal.fraSynTreeSelectElemen(var elem: TxpElement);
begin
  fraEditView1.SelectOrLoad(elem.srcDec, false);
end;
procedure TfrmPrincipal.fraSynTreeOpenFile(filname: string);
{El explorador de código, solicita abrir un archivo.}
begin
  fraEditView1.LoadFile(filname);
  Config.SaveToFile;  //guarda la configuración actual
end;
procedure TfrmPrincipal.fraSynTreeSelecFileExplorer;
{Se ha seleccionado el modo de explorador de archivo,}
var
  ed: TSynEditor;
begin
  //Ubica el archivo actual en el explorador.
  ed := fraEditView1.ActiveEditor;
  if (ed<>nil) and (ed.FileName<>'') then begin
     fraSynTree.LocateFile(ed.FileName);
  end;
end;
procedure TfrmPrincipal.cxp_RequireFileString(FilePath: string; var strList: TStrings);
{El compilador está solicitando acceder a un STringList, con el contenido de "FilePath",
para evitar tener que leerlo de disco, y ahcer más rápido el acceso.}
var
  i: Integer;
  ed: TSynEditor;
begin
  i := fraEditView1.SearchEditorIdx(FilePath);
  if i <> -1 then begin
    //Tiene el archivo abierto. Pasa la referencia.
    ed := fraEditView1.editors[i];
    if cxp.Compiling then ed.SaveFile;   //En compilación gurada siempre los archivos afectados
    strList := ed.SynEdit.Lines;
  end;
end;
procedure TfrmPrincipal.fraEdit_ChangeEditorState(ed: TSynEditor);
{Se produjo una modificación en el editor "ed"}
begin
  if not cxp.Compiling then begin
    //En compilación no se activa la verificación automática de sintaxis
    ticSynCheck := 0;  //reinicia cuenta
  end;
  acArcSave.Enabled := ed.Modified;
  acEdUndo.Enabled  := ed.CanUndo;
  acEdRedo.Enabled  := ed.CanRedo;
  //Para estas acciones no es necesario controlarlas, porque son acciones pre-determinadas
//  acEdiCortar.Enabled := edit.canCopy;
//  acEdiCopiar.Enabled := edit.canCopy;
//  acEdiPegar.Enabled  := edit.CanPaste;
  ed.ClearMarkErr;  //Quita la marca de error que pudiera haber
end;
procedure TfrmPrincipal.fraEdit_SelectEditor;
{Se ha cambiado el estado de los editores: Se ha cambaido la selección, se ha
agregado o eliminado alguno.}
var
  ed: TSynEditor;
begin
  //Se trata de realizar solo las tareas necesarias. Para no cargar el proceso.
  if fraEditView1.Count = 0 then begin
    //No hay ventanas de edición abiertas
//    fraEditView1.Visible := false;
    acArcSaveAs.Enabled := false;
    acEdSelecAll.Enabled := false;

    acArcSave.Enabled := false;
    acEdUndo.Enabled  := false;
    acEdRedo.Enabled  := false;

    StatusBar1.Panels[3].Text := '';
    StatusBar1.Panels[4].Text := '';
  end else begin
    //Hay ventanas de edición abiertas
    ed := fraEditView1.ActiveEditor;
    acArcSaveAs.Enabled := true;
    acEdSelecAll.Enabled := true;

    fraEdit_ChangeEditorState(ed);  //Actualiza botones

    StatusBar1.Panels[3].Text := ed.CodArc;  //Codificación
    StatusBar1.Panels[4].Text := ed.FileName;  //Nombre de archivo
  end;
  editChangeFileInform;
end;
procedure TfrmPrincipal.fraEdit_RequireSynEditConfig(ed: TsynEdit);
{Se pide actualizar la configuración de un editor.}
begin
  ed.PopupMenu := PopupEdit;
  Config.ConfigEditor(ed);
end;
procedure TfrmPrincipal.fraEdit_RequireSetCompletion(ed: TSynEditor);
{SOlicita configurar el completado de código al resaltador}
begin
  ct.SetCompletion(ed);
end;
procedure TfrmPrincipal.fraMessagesStatisDBlClick;
//Doble clcik en la sección de estadísticas
begin

end;
procedure TfrmPrincipal.cxp_AfterCompile;
{Se genera después de realizar la compilación.}
begin
  //Refresca el árbol de sintaxis, para actualizar la estructura del árbol de sintaxis
  if fraSynTree.Visible then begin
    fraSynTree.Refresh;
  end;
end;
procedure TfrmPrincipal.ConfigExtTool_RequirePar(var comLine: string);
{Se pide reemplazar parámetros en línea de comandos de Herramienta externa.}
begin
  comLine := StringReplace(comLine, '$(hexFile)', cxp.hexFilePath, [rfReplaceAll, rfIgnoreCase]);
  comLine := StringReplace(comLine, '$(mainFile)', cxp.mainFilePath, [rfReplaceAll, rfIgnoreCase]);
  comLine := StringReplace(comLine, '$(mainPath)', ExtractFileDir(cxp.mainFilePath), [rfReplaceAll, rfIgnoreCase]);
  comLine := StringReplace(comLine, '$(picModel)', cxp.PicName, [rfReplaceAll, rfIgnoreCase]);
end;
procedure TfrmPrincipal.LoadAsmSyntaxEd;
{Carga archivo de sinatxis para el editor de ASM}
var
  synFile: String;
begin
  synFile := rutSyntax + DirectorySeparator + 'PicPas_AsmPic.xml';
  if FileExists(synFile) then begin
    hlAssem.LoadFromFile(synFile);
  end else begin
    MsgErr('Syntax file not found: ' + synFile);
  end;
end;
procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
  fraSynTree := TfraSyntaxTree.Create(self);
  fraSynTree.Parent := self;
  //configura panel de mensajes
  fraMessages := TfraMessagesWin.Create(self);
  fraMessages.Parent := panMessages;  //Ubica
  fraMessages.Align := alClient;
  fraMessages.OnDblClickMessage := @fraMessagesDblClickMessage;
  fraMessages.OnStatisDBlClick  := @fraMessagesStatisDBlClick;
  //configura panel de edición
  fraEditView1 := TfraEditView.Create(self);
  fraEditView1.Parent := self;
  fraEditView1.OnChangeEditorState    := @fraEdit_ChangeEditorState;
  fraEditView1.OnSelectEditor         := @fraEdit_SelectEditor;
  fraEditView1.OnRequireSynEditConfig := @fraEdit_RequireSynEditConfig;
  fraEditview1.OnRequireSetCompletion := @fraEdit_RequireSetCompletion;
  //COnfigura Árbol de sintaxis
  fraSynTree.OnSelectElemen := @fraSynTreeSelectElemen;
  fraSynTree.OnOpenFile := @fraSynTreeOpenFile;
  fraSynTree.OnSelecFileExplorer := @fraSynTreeSelecFileExplorer;
  //Carga un resaltador a la ventana de ensamblador
  hlAssem := TSynFacilSyn.Create(self);
  edAsm.Highlighter := hlAssem;
  LoadAsmSyntaxEd;
  CT := TCodeTool.Create(fraEditView1, cxp, fraSynTree);
  cxp.OnRequireFileString := @cxp_RequireFileString;
  cxp.OnAfterCompile      := @cxp_AfterCompile;
  //Crea dinámicamente para poder inciailizarlo con comodidad
  frmDebug:= TfrmDebugger.Create(self);
  frmDebug.pic := cxp.pic;
end;
procedure TfrmPrincipal.FormShow(Sender: TObject);
var
  Hay: Boolean;
  SR: TSearchRec;
  srcFile: String;
begin
  fraSynTree.Align := alLeft;
  fraSynTree.Visible := true;
  splSynTree.Align := alLeft;
  AnchorTo(splSynTree, akLeft, fraSynTree);
  edAsm.Align := alRight;
  InicEditorC1(edAsm);
  splEdPas.Align := alRight;
  fraEditView1.Align := alClient;
  fraSynTree.Init(cxp.TreeElems);
  fraEditView1.tmpPath := rutTemp;   //fija ruta de trabajo
  Config.Iniciar;   //necesario para poder trabajar
  Config.OnPropertiesChanges := @ChangeAppearance;
  Config.fraCfgExtTool.OnReplaceParams := @ConfigExtTool_RequirePar;
  //Verifica si se llamó por línea de comando
  if Paramcount>0 then begin
    //Hay línea de comando
    srcFile := ParamStr(1);  //Lee archivo fuente
    {Es facil compilar por línea de comando en una aplicación para windows. Lo difícil
    es mostrar mensajes en la consola, proque no hay un terminal asociado. Usar writeln()
    generaría error y debugln(), solo funciona cuando se compila como aplicación de
    consola. Quizá lo mejor sería crear otro modo de compilación que sesa de consola,
    para que funcione bien la compilación por línea de comando.'}
    cxp.Compile(srcFile, true);
    if cxp.HayError then begin
      MsgBox('Compilation with errors.')
    end else begin
      MsgBox('Compilation successful.');
    end;
    self.Close;
  end;
  //Termina configuración
  fraEditView1.InitMenuRecents(mnRecents, Config.fraCfgSynEdit.ArcRecientes);  //inicia el menú "Recientes"
  ChangeAppearance;   //primera actualización
  //Carga lista de ejemplos
  Hay := FindFirst(rutSamples + DirectorySeparator + '*.pas', faAnyFile - faDirectory, SR) = 0;
  while Hay do begin
     //encontró archivo
    AddItemToMenu(mnSamples, '&'+ChangeFileExt(SR.name,''),@DoSelectSample);
    Hay := FindNext(SR) = 0;
  end;
  //Inicia encabezado
  //Carga último archivo
  if Config.LoadLast then fraEditView1.LoadListFiles(Config.filesClosed);
end;
procedure TfrmPrincipal.DoSelectSample(Sender: TObject);
//Se ha seleccionado un archivo de ejemplo.
var
  SamFil: String;
  it: TMenuItem;
begin
  it := TMenuItem(Sender);
  SamFil := rutSamples + DirectorySeparator + it.Caption + '.pas';
  SamFil := StringReplace(SamFil,'&','',[rfReplaceAll]);
  //Carga archivo
  fraEditView1.LoadFile(SamFil);
end;
procedure TfrmPrincipal.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  lstClosedFiles: string;
begin
  if curProj<>nil then begin
    if not curProj.Close then begin
      CanClose := False;  //Cancela
      exit;
    end;
    curProj.Destroy;
  end;
  if fraEditView1.CloseAll(lstClosedFiles) then begin
     CanClose := false;   //cancela
  end else begin
    //Se va a cerrar. Guarda la lista de archivos quee staban abiertos
    Config.filesClosed := lstClosedFiles;
  end;
end;
procedure TfrmPrincipal.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Config.SynTreeWidth := fraSynTree.Width;   //Guarda ancho
  Config.SaveToFile;  //guarda la configuración actual
end;
procedure TfrmPrincipal.FormDestroy(Sender: TObject);
begin
  frmDebug.Destroy;
  CT.Destroy;
  hlAssem.Free;
end;
procedure TfrmPrincipal.Timer1Timer(Sender: TObject);
var
  ed: TSynEditor;
begin
  inc(tic);
  inc(ticSynCheck);
  if (tic mod 5 = 0) and StatusBar1.Visible then begin
    //Cada 0.5 seg, se actualiza la barra de estado
    if fraEditView1.Count = 0 then begin
      //No hay editores
       StatusBar1.Panels[0].Text := ''
    end else begin
      //Hay archivos abiertos
      ed := fraEditView1.ActiveEditor;
      //Actualiza Barra de estado
      if ed.Modified then
        StatusBar1.Panels[0].Text := MSG_MODIF
      else
        StatusBar1.Panels[0].Text := MSG_SAVED;
      //Actualiza cursor
      StatusBar1.Panels[1].Text := Format('%d,%d', [ed.SynEdit.CaretX, ed.SynEdit.CaretY]);
    end;
  end;
  if Config.AutSynChk and (ticSynCheck = 5) then begin
    //Se cumplió el tiempo para iniciar la verificación automática de sintaxis
//    debugln('--Verif. Syntax.' + TimeToStr(now));
    if fraEditView1.Count>0 then begin
      //Hay archivo abiertos
      ed := fraEditView1.ActiveEditor;
      if (ed.SynEdit.Lines.Count <=1) and  (trim(ed.Text)='') then begin
        //Verifica rápidamente si hay texto en el editor
         fraMessages.InitCompilation(cxp, false);  //Limpia mensajes
        exit;
      end;
      fraMessages.InitCompilation(cxp, false);  //Limpia mensajes
      cxp.Compile(ed.FileName, false);
      //Puede haber generado error, los mismos que deben haberse mostrado en el panel.
      MarkErrors;  //Resalta errores, si están en el editor actual
      fraMessages.FilterGrid;  //Para que haga visible la lista de mensajes
    end;
  end;
end;
procedure TfrmPrincipal.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  i: Integer;
begin
  //Carga archivo arrastrados
  for i:=0 to high(FileNames) do begin
    fraEditView1.LoadFile(FileNames[i]);
  end;
end;
procedure TfrmPrincipal.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  {Realmente, todo este código podrái ir dentro de de CT.KeyDown.}
  if (Shift = [ssCtrl]) and (Key = VK_TAB) then begin
    if fraEditView1.HasFocus then begin
      if fraEditView1.Count>1 then begin
        fraEditView1.SelectNextEditor;
      end else begin
        //Debe haber solo una ventana
        if edAsm.Visible then edAsm.SetFocus;
      end;
    end else if edAsm.Focused then begin
      fraEditView1.SetFocus;
    end;
  end;
  if (Shift = [ssShift, ssCtrl]) and (Key = VK_TAB) then begin
    if fraEditView1.HasFocus then fraEditView1.SelectPrevEditor;
  end;
  if (Shift = [ssCtrl]) and (Key = VK_F4) then begin
    if fraEditView1.HasFocus then acArcCloseFileExecute(self);
    Shift := []; Key := 0;  //para qie no pase
  end;
  //Pasa evento a COde Tool
  CT.KeyDown(Sender, Key, Shift);
end;
procedure TfrmPrincipal.fraMessagesDblClickMessage(const srcPos: TSrcPos);
begin
  fraEditView1.SelectOrLoad(srcPos, false);
end;
procedure TfrmPrincipal.ChangeAppearance;
//Se han cambiado las opciones de configuración.
  procedure SetStateActionsProject(state: boolean);
  begin
    acArcSave.Enabled := state;
    acArcCloseProj.Enabled := state;
    acEdUndo.Enabled := state;
    acEdRedo.Enabled := state;
    acEdCut.Enabled := state;
    acEdCopy.Enabled := state;
    acEdPaste.Enabled := state;
  end;
var
  cad: String;
  i: Integer;
  tool : TExternTool;
begin
  SetLanguage(copy(Config.language, 1, 2));
  if curProj = nil then begin
    SetStateActionsProject(false);
//    exit;
  end else begin
    SetStateActionsProject(true);
  end;
  //Visibilidad del explorador de código
  fraSynTree.Visible := Config.ViewSynTree;
  fraSynTree.Width   := Config.SynTreeWidth;
  splSynTree.Visible := Config.ViewSynTree;
  acViewSynTree.Checked := Config.ViewSynTree;

  //Visibilidad de La Barra de Estado
  StatusBar1.Visible := Config.ViewStatusbar;
  acViewStatbar.Checked:= Config.ViewStatusbar;

  //Visibilidad de la Barra de Herramientas
  ToolBar1.Visible   := Config.ViewToolbar;
  acViewToolbar.Checked:= Config.ViewToolbar;

  //Visibilidad del Panel de Mensajes
  panMessages.Visible:= Config.ViewPanMsg;
  Splitter2.Visible  := Config.ViewPanMsg;
  acViewMsgPan.Checked:= Config.ViewPanMsg;

  //Visibilidad del Visor de Ensamblador
  edAsm.Visible      := Config.ViewPanAssem;
  splEdPas.Visible   := Config.ViewPanAssem;
  acViewAsmPan.Checked:= Config.ViewPanAssem;
  //Tamaño de la Barra de Herramientas
  case Config.StateToolbar of
  stb_SmallIcon: begin
    ToolBar1.ButtonHeight:=22;
    ToolBar1.ButtonWidth:=22;
    ToolBar1.Height:=26;
    ToolBar1.Images:=ImgActions16;
  end;
  stb_BigIcon: begin
    ToolBar1.ButtonHeight:=38;
    ToolBar1.ButtonWidth:=38;
    ToolBar1.Height:=42;
    ToolBar1.Images:=ImgActions32;
  end;
  end;
  //Configura Explorador de código
  fraSynTree.BackColor := Config.CodExplBack;;
  fraSynTree.TextColor := Config.CodExplText;
  fraSynTree.frmArcExplor1.Filter.ItemIndex := Config.cexpFiltype;
  fraSynTree.frmArcExplor1.FilterChange(self);
  //Configura Visor de Mensajes
  fraMessages.BackColor := Config.MessPanBack;
  fraMessages.TextColor := Config.MessPanText;
  fraMessages.TextErrColor := Config.MessPanErr;
  fraMessages.BackSelColor := Config.MessPanSel;

  fraMessages.PanelColor := Config.PanelsCol;
  ToolBar1.Color := Config.PanelsCol;
  fraEditView1.Panel1.Color := Config.PanelsCol;
  //fraEditView1.Color :=  Config.PanelsCol;
  //Color de separadores
  Splitter2.Color := Config.SplitterCol;
  splSynTree.Color := Config.SplitterCol;
  splEdPas.Color := Config.SplitterCol;
  //Configura editor ASM
  Config.ConfigEditor(edAsm);
  LoadAsmSyntaxEd;
  //Solicita configura los editores activos
  fraEditView1.UpdateSynEditConfig;
  fraEditView1.TabViewMode := Config.TabEdiMode;
  //Configura accesos a herramientas externas.
  //Solo es aplicable a las primeras 5 herramientas
  acToolExt1.Visible := false;
  acToolExt2.Visible := false;
  acToolExt3.Visible := false;
  acToolExt4.Visible := false;
  acToolExt5.Visible := false;
  for i:=0 to config.fraCfgExtTool.ExternTools.Count-1 do begin
    cad := config.fraCfgExtTool.ExternTools[i];
    tool.ReadFromString(cad);  //lee campos
    case i of
    0: if tool.ShowInTbar then begin
         acToolExt1.Visible := true;
         acToolExt1.Caption:= tool.name;
         acToolExt1.Hint := tool.name;
       end;
    1: if tool.ShowInTbar then begin
         acToolExt2.Visible := true;
         acToolExt2.Caption:= tool.name;
         acToolExt2.Hint := tool.name;
       end;
    2: if tool.ShowInTbar then begin
         acToolExt3.Visible := true;
         acToolExt3.Caption:= tool.name;
         acToolExt3.Hint := tool.name;
       end;
    3: if tool.ShowInTbar then begin
         acToolExt4.Visible := true;
         acToolExt4.Caption:= tool.name;
         acToolExt4.Hint := tool.name;
       end;
    4: if tool.ShowInTbar then begin
         acToolExt5.Visible := true;
         acToolExt5.Caption:= tool.name;
         acToolExt5.Hint := tool.name;
       end;
    end;
  end;
end;
procedure TfrmPrincipal.editChangeFileInform;
{Actualiza la barra de título, de acuerdo al estado}
var
  ed: TSynEditor;
begin
  ed := fraEditView1.ActiveEditor;
  if curProj= nil then begin
    //Modo de archivos. Actualiza nombre de archivo
    if fraEditView1.Count = 0 then begin
      Caption := NOM_PROG + ' - ' + VER_PROG  + ' - ' +MSG_NOFILES;
    end else begin  //Hay varios
      if ed.FileName='' then
        Caption := NOM_PROG + ' - ' + VER_PROG  + ' - ' + ed.Caption
      else
        Caption := NOM_PROG + ' - ' + VER_PROG  + ' - ' + ed.FileName;
    end;
  end else begin
    //Hay un proyecto abierto
    Caption := NOM_PROG + ' - ' + VER_PROG  + ' - Project: ' + curProj.name;
  end;
  if (ed<>nil) and (ed.FileName<>'') then begin
     fraSynTree.LocateFile(ed.FileName);
  end;
end;
procedure TfrmPrincipal.FindDialog1Find(Sender: TObject);
var
  encon  : integer;
  buscado : string;
  opciones: TSynSearchOptions;
  curEdit: TSynEdit;
begin
  if fraEditView1.ActiveEditor = nil then exit;
  curEdit := fraEditView1.ActiveEditor.SynEdit;
  buscado := FindDialog1.FindText;
  opciones := [];
  if not(frDown in FindDialog1.Options) then opciones += [ssoBackwards];
  if frMatchCase in FindDialog1.Options then opciones += [ssoMatchCase];
  if frWholeWord in FindDialog1.Options then opciones += [ssoWholeWord];
  if frEntireScope in FindDialog1.Options then opciones += [ssoEntireScope];
  encon := curEdit.SearchReplace(buscado,'',opciones);
  if encon = 0 then
     MsgBox(MSG_NOFOUND_, [buscado]);
end;
procedure TfrmPrincipal.ReplaceDialog1Replace(Sender: TObject);
var
  encon, r : integer;
  buscado : string;
  opciones: TSynSearchOptions;
  curEdit: TSynEdit;
begin
  if fraEditView1.ActiveEditor = nil then exit;
  curEdit := fraEditView1.ActiveEditor.SynEdit;
  buscado := ReplaceDialog1.FindText;
  opciones := [ssoFindContinue];
  if not(frDown in ReplaceDialog1.Options) then opciones += [ssoBackwards];
  if frMatchCase in ReplaceDialog1.Options then opciones += [ssoMatchCase];
  if frWholeWord in ReplaceDialog1.Options then opciones += [ssoWholeWord];
  if frEntireScope in ReplaceDialog1.Options then opciones += [ssoEntireScope];
  if frReplaceAll in ReplaceDialog1.Options then begin
    //se ha pedido reemplazar todo
    encon := curEdit.SearchReplace(buscado,ReplaceDialog1.ReplaceText,
                              opciones+[ssoReplaceAll]);  //reemplaza
    MsgBox(MSG_N_REPLAC, [IntToStr(encon)]);
    exit;
  end;
  //reemplazo con confirmación
  ReplaceDialog1.CloseDialog;
  encon := curEdit.SearchReplace(buscado,'',opciones);  //búsqueda
  while encon <> 0 do begin
      //pregunta
      r := Application.MessageBox(pChar(MSG_REPTHIS), '', MB_YESNOCANCEL);
      if r = IDCANCEL then exit;
      if r = IDYES then begin
        curEdit.TextBetweenPoints[curEdit.BlockBegin,curEdit.BlockEnd] := ReplaceDialog1.ReplaceText;
      end;
      //busca siguiente
      encon := curEdit.SearchReplace(buscado,'',opciones);  //búsca siguiente
  end;
  MsgBox(MSG_NOFOUND_, [buscado]);
end;
/////////////////// Acciones de Archivo /////////////////////
procedure TfrmPrincipal.acArcNewFileExecute(Sender: TObject);
begin
  fraEditView1.NewPasFile;
  with fraEditView1.ActiveEditor.SynEdit.Lines do begin
    Add('////////////////////////////////////////////');
    Add('// New program created in ' + DateToStr(now) + '}');
    Add('////////////////////////////////////////////');
    Add('program NewProgram;');
    Add('{$PROCESSOR PIC16F84A}');
    Add('{$FREQUENCY 4MHZ}');
    Add('uses PIC16F84A;');
    Add('  ');
    Add('//Declarations here');
    Add('  ');
    Add('begin');
    Add('  ');
    Add('  //Code here');
    Add('  ');
    Add('end.');
  end;
  fraEditView1.SetFocus;
end;
procedure TfrmPrincipal.acArcNewProjExecute(Sender: TObject);
begin
  if curProj<>nil then begin
    if not curProj.Close then exit;
    curProj.Destroy;
  end;
  curProj := TPicPasProject.Create;
  curProj.Open;
  fraEditView1.NewPasFile;
end;
procedure TfrmPrincipal.acArcOpenExecute(Sender: TObject);
begin
  fraEditView1.OpenDialog;
  fraEditView1.SetFocus;
  Config.SaveToFile;  //para que guarde el nombre del último archivo abierto
end;
procedure TfrmPrincipal.acArcCloseFileExecute(Sender: TObject);
begin
  fraEditView1.CloseEditor;
  fraEditView1.SetFocus;
end;
procedure TfrmPrincipal.acArcCloseProjExecute(Sender: TObject);
begin
  if curProj<>nil then begin
    if not curProj.Close then exit;
    curProj.Destroy;
  end;
end;
procedure TfrmPrincipal.acArcSaveExecute(Sender: TObject);
begin
  fraEditView1.SaveFile;
end;
procedure TfrmPrincipal.acArcSaveAsExecute(Sender: TObject);
begin
  fraEditView1.SaveAsDialog;
end;
procedure TfrmPrincipal.acArcQuitExecute(Sender: TObject);
begin
  frmPrincipal.Close;
end;
//////////// Acciones de Edición ////////////////
procedure TfrmPrincipal.acEdiUndoExecute(Sender: TObject);
begin
  if fraEditView1.ActiveEditor= nil then exit;
  fraEditView1.ActiveEditor.Undo;
end;
procedure TfrmPrincipal.acEdRedoExecute(Sender: TObject);
begin
  fraEditView1.Redo;
end;
procedure TfrmPrincipal.acEdSelecAllExecute(Sender: TObject);
begin
  fraEditView1.SelectAll;
end;
procedure TfrmPrincipal.acEdUndoExecute(Sender: TObject);
begin
  fraEditView1.Undo;
end;
procedure TfrmPrincipal.acEdiRedoExecute(Sender: TObject);
begin
  if fraEditView1.ActiveEditor= nil then exit;
  fraEditView1.ActiveEditor.Redo;
end;
procedure TfrmPrincipal.acEdiSelecAllExecute(Sender: TObject);
begin
  if fraEditView1.ActiveEditor= nil then exit;
  fraEditView1.ActiveEditor.SelectAll;
end;
//////////// Acciones de Búsqueda ////////////////
procedure TfrmPrincipal.acSearFindExecute(Sender: TObject);
begin
  FindDialog1.Execute;
end;
procedure TfrmPrincipal.acSearFindNxtExecute(Sender: TObject);
begin
  FindDialog1Find(self);
end;
procedure TfrmPrincipal.acSearFindPrvExecute(Sender: TObject);
begin
  if frDown in FindDialog1.Options then begin
    FindDialog1.Options := FindDialog1.Options - [frDown];  //Quita
    FindDialog1Find(self);
    FindDialog1.Options := FindDialog1.Options + [frDown];  //Restaura
  end else begin
    FindDialog1Find(self);
  end;
end;
procedure TfrmPrincipal.acSearReplacExecute(Sender: TObject);
begin
  ReplaceDialog1.Execute;
end;
//////////// Acciones de Ver ///////////////
procedure TfrmPrincipal.acViewStatbarExecute(Sender: TObject);
begin
  Config.ViewStatusbar:=not Config.ViewStatusbar;
end;
procedure TfrmPrincipal.acViewToolbarExecute(Sender: TObject);
begin
  Config.ViewToolbar:= not Config.ViewToolbar;
end;
procedure TfrmPrincipal.acViewMsgPanExecute(Sender: TObject);
begin
  Config.ViewPanMsg:= not Config.ViewPanMsg;
end;
procedure TfrmPrincipal.acViewSynTreeExecute(Sender: TObject);
begin
  Config.ViewSynTree := not Config.ViewSynTree;
end;
procedure TfrmPrincipal.acViewAsmPanExecute(Sender: TObject);
begin
  Config.ViewPanAssem := not Config.ViewPanAssem;
end;
//////////// Acciones de Herramientas ///////////////
procedure TfrmPrincipal.acToolCompilExecute(Sender: TObject);
{Compila el contenido del archivo actual}
var
  filName: String;
begin
  if fraEditView1.ActiveEditor=nil then exit;
  self.SetFocus;
  filName := fraEditView1.ActiveEditor.FileName;
  if filName='' then begin
    //No tiene nombre. No debería pasar, porque "fraEditView1" debe generar nombres.
    if fraEditView1.SaveAsDialog then begin
      MsgExc('File must be saved before compiling.');
      exit;
    end;
  end;
  fraMessages.InitCompilation(cxp, true);  //Limpia mensajes
  cxp.incDetComm   := Config.IncComment2;   //Visualización de mensajes
  cxp.SetProIniBnk := not Config.OptBnkBefPro;
  cxp.OptBnkAftPro := Config.OptBnkAftPro;
  cxp.OptBnkAftIF  := Config.OptBnkAftIF;
  cxp.OptReuProVar := Config.ReuProcVar;
  cxp.OptRetProc   := Config.OptRetProc;
  ticSynCheck := 1000; //Desactiva alguna Verif. de sintaxis, en camino.
  cxp.Compiling := true;   //Activa bandera
  cxp.Compile(filName);
  cxp.Compiling := false;
  if fraMessages.HaveErrors then begin
    fraMessages.EndCompilation;
    VerificarError;
    MarkErrors;
    exit;
  end;
  fraMessages.EndCompilation;
  //Genera código ensamblador
  edAsm.BeginUpdate(false);
  edAsm.Lines.Clear;
  if Config.IncHeadMpu then begin
    //Incluye encabezado
     edAsm.Lines.Add('    list     p='+ cxp.PicNameShort);
     edAsm.Lines.Add('    #include <p' + cxp.PicNameShort + '.inc>');
//     edAsm.Lines.Add('    __CONFIG        _CP_OFF & _PWRTE_ON & _WDT_OFF & _XT_OSC');
  end;
  if Config.IncVarDec then begin
     edAsm.Lines.Add(';===RAM usage===');
     cxp.RAMusage(edAsm.Lines, Config.VarDecType, Config.ExcUnused);
  end;
  edAsm.Lines.Add(';===Blocks of Code===');
  cxp.DumpCode(edAsm.Lines, Config.IncAddress, true, COnfig.IncVarName );
  edAsm.Lines.Add(';--------------------');
  edAsm.Lines.Add('    END');
  edAsm.EndUpdate;
end;
procedure TfrmPrincipal.acToolASMDebugExecute(Sender: TObject);
begin
  frmDebug.Exec;
end;
procedure TfrmPrincipal.acToolPICExplExecute(Sender: TObject);
begin
  frmPICExplorer.Show;
end;
procedure TfrmPrincipal.acToolRamExpExecute(Sender: TObject);
begin
   frmRAMExplorer.Exec(cxp.pic);
end;
procedure TfrmPrincipal.acToolConfigExecute(Sender: TObject);
begin
  Config.Mostrar;
end;
procedure TfrmPrincipal.acToolListRepExecute(Sender: TObject);
{Muestra un conteo de instrucciones.}
var
  edit: TSynEditor;
begin
  fraEditView1.NewLstFile;
  edit := fraEditView1.ActiveEditor;
  edit.SynEdit.BeginUpdate;
  cxp.GenerateListReport(edit.SynEdit.Lines);
  edit.SynEdit.EndUpdate;
end;
procedure TfrmPrincipal.acToolExt1Execute(Sender: TObject);
begin
  Config.fraCfgExtTool.ExecTool(0);
end;
procedure TfrmPrincipal.acToolExt2Execute(Sender: TObject);
begin
  Config.fraCfgExtTool.ExecTool(1);
end;
procedure TfrmPrincipal.acToolExt3Execute(Sender: TObject);
begin
  Config.fraCfgExtTool.ExecTool(2);
end;
procedure TfrmPrincipal.acToolExt4Execute(Sender: TObject);
begin
  Config.fraCfgExtTool.ExecTool(3);
end;
procedure TfrmPrincipal.acToolExt5Execute(Sender: TObject);
begin
  Config.fraCfgExtTool.ExecTool(4);
end;
procedure TfrmPrincipal.acToolFindDecExecute(Sender: TObject);
{Ubica la declaración del elemento}
begin
  if fraEditView1.Count=0 then exit;
  ct.GoToDeclaration;
end;
//Adicionales
procedure TfrmPrincipal.MarkErrors;
{Marca los errores del panel de mensajes, en la ventana activa del editor.
Los erroes solo se marcarán si es que se udican en la ventana activa del editor.}
var
  msg, filname: string;
  row, col, f: integer;
  ed: TSynEditor;
begin
  ed := fraEditView1.ActiveEditor;
  if fraMessages.HaveErrors then begin
    //Obtiene las coordenadas de los errores
     for f:=1 to fraMessages.grilla.RowCount -1 do begin
       if fraMessages.IsErroridx(f) then begin
         fraMessages.GetErrorIdx(f, msg, filname, row, col);  //obtiene información del error
         if (msg<>'') and (filname = ed.FileName) then begin
           //Hay error en el archivo actual
           ed.MarkError(Point(col, row));
         end;
       end;
     end;

//     fraMessages.GetFirstError(msg, filname, row, col);
//     if (msg<>'') and (filname = ed.FileName) then begin
//       //Hay error en el archivo actual
//       ed.MarkError(Point(col, row));
//     end;
  end;
end;
procedure TfrmPrincipal.VerificarError;
//Verifica si se ha producido algún error en el preprocesamiento y si lo hay
//Ve la mejor forma de msotrarlo
var
  msg, filname: string;
  row, col: integer;
begin
    fraMessages.GetFirstError(msg, filname, row, col);
    if msg='' then exit;
    //Selecciona posición de error en el Editor
    if filname <> '' Then begin
        fraEditView1.SelectOrLoad(filname);  //Selecciona o abre
         //Ya lo tenemos cargado
        If row <> -1 Then begin
           MarcarError(fraEditView1.ActiveEditor, row, col);
        end;
        if Config.ShowErMsg Then MsgErr(msg);
    end else begin   //no hay archivo de error
      if Config.ShowErMsg Then MsgErr(msg);
    end;
End;
procedure TfrmPrincipal.MarcarError(ed: TSynEditor; nLin, nCol: integer);
begin
  fraEditView1.SetFocus;
  //posiciona curosr
//  ed.SynEdit.CaretY := nLin; //primero la fila
//  ed.SynEdit.CaretX := nCol;
  ed.SynEdit.LogicalCaretXY := Point(nCol, nLin);
  //Define línea con error
  ed.linErr := nLin;
  ed.SynEdit.Invalidate;  //refresca
end;

end.

