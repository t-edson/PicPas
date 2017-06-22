{                                   PicPas.
Compilador en Pascal para micorocntroladores PIC de la serie 16.

                                        Por Tito Hinostroza   22/08/2015 }
unit FormPrincipal;
{$mode objfpc}{$H+}
{$define }
interface
uses
  Classes, SysUtils, SynEdit, Forms, Controls, Dialogs, Menus, ComCtrls,
  ActnList, StdActns, ExtCtrls, LCLIntf, LCLType, LCLProc, SynFacilHighlighter,
  SynFacilUtils, MisUtils, XpresBas, Parser, FormPICExplorer, Globales,
  FrameSyntaxTree, FormConfig, PicPasProject, FrameEditView,
  FrameMessagesWin, XpresElementsPIC, ProcAsm;
type
  { TfrmPrincipal }
  TfrmPrincipal = class(TForm)
    acArcOpen: TAction;
    acArcSaveAs: TAction;
    acArcSave: TAction;
    acArcNewFile: TAction;
    acArcQuit: TAction;
    acBusFind: TAction;
    acBusFindNxt: TAction;
    acBusReplac: TAction;
    acEdCopy: TEditCopy;
    acEdCut: TEditCut;
    acEdPaste: TEditPaste;
    acEdRedo: TAction;
    acEdSelecAll: TAction;
    acEdUndo: TAction;
    acArcNewProj: TAction;
    acArcCloseProj: TAction;
    acArcCloseFile: TAction;
    acToolConfig2: TAction;
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
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton2: TToolButton;
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
    procedure acEdiRedoExecute(Sender: TObject);
    procedure acEdiSelecAllExecute(Sender: TObject);
    procedure acEdiUndoExecute(Sender: TObject);
    procedure acEdRedoExecute(Sender: TObject);
    procedure acEdSelecAllExecute(Sender: TObject);
    procedure acEdUndoExecute(Sender: TObject);
    procedure acToolCompilExecute(Sender: TObject);
    procedure acToolConfigExecute(Sender: TObject);
    procedure acToolPICExplExecute(Sender: TObject);
    procedure acViewSynTreeExecute(Sender: TObject);
    procedure acViewStatbarExecute(Sender: TObject);
    procedure acViewToolbarExecute(Sender: TObject);
    procedure acViewMsgPanExecute(Sender: TObject);
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
    procedure Timer1Timer(Sender: TObject);
  private
    curProj     : TPicPasProject; //Proyecto actual
    hlAssem     : TSynFacilSyn;   //resaltador para ensamblador
    fraEditView1: TfraEditView;   //Panel de editores
    fraSynTree  : TfraSyntaxTree; //Árbol de sintaxis
    fraMessages : TfraMessagesWin;
    procedure fraSynTreeSelecFileExplorer;
    procedure fraEdit_RequireSynEditConfig(ed: TsynEdit);
    procedure ChangeAppearance;
    procedure fraEdit_SelectEditor;
    procedure fraMessagesDblClickMessage(const srcPos: TSrcPos);
    procedure fraSynTreeOpenFile(filname: string);
    procedure fraSynTreeSelectElemen(var elem: TxpElement);
    procedure MarcarError(ed: TSynEditor; nLin, nCol: integer);
    procedure VerificarError;
  public
    procedure SetLanguage(idLang: string);
  end;

var
  frmPrincipal: TfrmPrincipal;

implementation
{$R *.lfm}
{ TfrmPrincipal }
procedure TfrmPrincipal.SetLanguage(idLang: string);
begin
  Config.SetLanguage(idLang);
  fraSynTree.SetLanguage(idLang);
  fraEditView1.SetLanguage(idLang);
  fraMessages.SetLanguage(idLang);
  Parser.SetLanguage(idLang);
  ProcAsm.SetLanguage(idLang);
  curLang := idLang;
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
end;
procedure TfrmPrincipal.fraSynTreeSelecFileExplorer;
{Se ha seleccionado el modo de explorador de archivo,}
var
  ed: TSynEditor;
begin
  //Ubica el archivo actual en el explorador.
  ed := fraEditView1.ActiveEditor;
  if (ed<>nil) and (ed.NomArc<>'') then begin
     fraSynTree.LocateFile(ed.NomArc);
  end;
end;
procedure TfrmPrincipal.FormCreate(Sender: TObject);
var
  synFile: String;
begin

  fraSynTree := TfraSyntaxTree.Create(self);
  fraSynTree.Parent := self;
  //configura panel de mensajes
  fraMessages := TfraMessagesWin.Create(self);
  fraMessages.Parent := panMessages;  //Ubica
  fraMessages.Align := alClient;
  fraMessages.OnDblClickMessage := @fraMessagesDblClickMessage;
  //configura panel de edición
  fraEditView1 := TfraEditView.Create(self);
  fraEditView1.Parent := self;
  fraEditView1.OnChangeEditorState := @fraEdit_ChangeEditorState;
  fraEditView1.OnSelectEditor := @fraEdit_SelectEditor;
  fraEditView1.OnRequireSynEditConfig := @fraEdit_RequireSynEditConfig;
  //COnfigura Árbol de sintaxis
  fraSynTree.OnSelectElemen := @fraSynTreeSelectElemen;
  fraSynTree.OnOpenFile := @fraSynTreeOpenFile;
  fraSynTree.OnSelecFileExplorer := @fraSynTreeSelecFileExplorer;
  //carga un resaltador a la ventana de ensamblador
  hlAssem := TSynFacilSyn.Create(self);
  edAsm.Highlighter := hlAssem;
  synFile := rutSyntax + DirectorySeparator + 'PicPas_AsmPic.xml';
  if FileExists(synFile) then begin
    hlAssem.LoadFromFile(synFile);
  end;
end;
procedure TfrmPrincipal.FormShow(Sender: TObject);
var
  Hay: Boolean;
  SR: TSearchRec;
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
  fraEditView1.InitMenuRecents(mnRecents, Config.fraCfgSynEdit.ArcRecientes);  //inicia el menú "Recientes"
  ChangeAppearance;   //primera actualización
  //carga lista de ejemplos
  Hay := FindFirst(rutSamples + DirectorySeparator + '*.pas', faAnyFile - faDirectory, SR) = 0;
  while Hay do begin
     //encontró archivo
    AddItemToMenu(mnSamples, '&'+ChangeFileExt(SR.name,''),@DoSelectSample);
    Hay := FindNext(SR) = 0;
  end;
  //Inicia encabezado
  //Carga último archivo
  if Config.LoadLast then fraEditView1.LoadLastFileEdited;
//  if FileExists('SinNombre.pas') then fraEditView1.LoadFile('SinNombre.pas');
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
begin
  if curProj<>nil then begin
    if not curProj.Close then begin
      CanClose := False;  //Cancela
      exit;
    end;
    curProj.Destroy;
  end;
  if fraEditView1.CloseAll then CanClose := false;   //cancela
end;
procedure TfrmPrincipal.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Config.SynTreeWidth := fraSynTree.Width;   //Guarda ancho
  Config.SaveToFile;  //guarda la configuración actual
end;
procedure TfrmPrincipal.FormDestroy(Sender: TObject);
begin
  hlAssem.Free;
end;
procedure TfrmPrincipal.Timer1Timer(Sender: TObject);
var
  ed: TSynEditor;
begin
  if fraEditView1.Count = 0 then begin
    //No hay editores
     StatusBar1.Panels[0].Text := ''
  end else begin
    //Hay archivos abiertos
    ed := fraEditView1.ActiveEditor;
    //Actualiza Barra de estado
    if ed.Modified then
      StatusBar1.Panels[0].Text := '(*)Modified'
    else
      StatusBar1.Panels[0].Text := 'Saved';
    //Actualiza cursor
    StatusBar1.Panels[1].Text := Format('%d,%d', [ed.SynEdit.CaretX, ed.SynEdit.CaretY]);
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
  if (Shift = [ssCtrl]) and (Key = VK_TAB) then begin
    if fraEditView1.HasFocus then begin
      if fraEditView1.Count>1 then begin
        fraEditView1.SelectNextEditor;
      end else begin
        //Debe haber solo una ventana
        edAsm.SetFocus;
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
end;
procedure TfrmPrincipal.fraEdit_ChangeEditorState(ed: TSynEditor);
begin
  acArcSave.Enabled := ed.Modified;
  acEdUndo.Enabled  := ed.CanUndo;
  acEdRedo.Enabled  := ed.CanRedo;
  //Para estas acciones no es necesario controlarlas, porque son acciones pre-determinadas
//  acEdiCortar.Enabled := edit.canCopy;
//  acEdiCopiar.Enabled := edit.canCopy;
//  acEdiPegar.Enabled  := edit.CanPaste;
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
    StatusBar1.Panels[4].Text := ed.NomArc;  //Nombre de archivo
  end;
  editChangeFileInform;
end;
procedure TfrmPrincipal.fraEdit_RequireSynEditConfig(ed: TsynEdit);
{Se pide actualizar la configuración de un editor.}
begin
  ed.PopupMenu := PopupEdit;
  Config.ConfigEditor(ed);
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
begin
  SetLanguage(copy(Config.language, 1, 2));
  if curProj = nil then begin
    SetStateActionsProject(false);
//    exit;
  end else begin
    SetStateActionsProject(true);
  end;

  fraSynTree.Visible := Config.ViewSynTree;
  fraSynTree.Width := Config.SynTreeWidth;
  splSynTree.Visible := Config.ViewSynTree;

  StatusBar1.Visible := Config.ViewStatusbar;
  acViewStatbar.Checked := Config.ViewStatusbar;

  ToolBar1.Visible := Config.ViewToolbar;
  acViewToolbar.Checked:= Config.ViewToolbar;

  panMessages.Visible:= Config.ViewPanMsg;
  Splitter2.Visible := Config.ViewPanMsg;
  acViewMsgPan.Checked:= Config.ViewPanMsg;

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
  fraSynTree.TextColor := Config.CodExplText;  //No funciona
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
  //Solicita configura los editores activos
  fraEditView1.UpdateSynEditConfig;
  fraEditView1.TabViewMode := Config.TabEdiMode;
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
      Caption := NOM_PROG + ' - ' + VER_PROG  + ' - No files.';
    end else begin  //Hay varios
      if ed.NomArc='' then
        Caption := NOM_PROG + ' - ' + VER_PROG  + ' - ' + ed.Caption
      else
        Caption := NOM_PROG + ' - ' + VER_PROG  + ' - ' + ed.NomArc;
    end;
  end else begin
    //Hay un proyecto abierto
    Caption := NOM_PROG + ' - ' + VER_PROG  + ' - Project: ' + curProj.name;
  end;
  if (ed<>nil) and (ed.NomArc<>'') then begin
     fraSynTree.LocateFile(ed.NomArc);
  end;
end;
/////////////////// Acciones de Archivo /////////////////////
procedure TfrmPrincipal.acArcNewFileExecute(Sender: TObject);
begin
  fraEditView1.NewFile;
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
  fraEditView1.NewFile;
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
//////////// Acciones de Herramientas ///////////////
procedure TfrmPrincipal.acToolCompilExecute(Sender: TObject);
{Compila el contenido del archivo actual}
var
  filName: String;
begin
  if fraEditView1.ActiveEditor=nil then exit;
  self.SetFocus;
  filName := fraEditView1.ActiveEditor.NomArc;
  if filName='' then begin
    //No tiene nombre. No debería pasar, porque "fraEditView1" debe generar nombres.
    if fraEditView1.SaveAsDialog then begin
      MsgExc('File must be saved before compiling.');
      exit;
    end;
  end else begin
    //Este es el caso normal
//    fraEditView1.SaveFile;  //Guarda primero el archivo
    fraEditView1.SaveAll;   //puede que se esté editando archivos que usa el programa
  end;
  fraMessages.InitCompilation(cxp);  //Limpia mensajes
  cxp.incDetComm := Config.IncComment2;   //Visualización de mensajes
  cxp.SetProIniBnk := Config.SetProIniBnk;
  cxp.SetProEndBnk := Config.SetProEndBnk;
  cxp.Compile(filName, fraEditView1.ActiveEditor.SynEdit.Lines);
  if cxp.HayError then begin
    fraMessages.EndCompilation;
    VerificarError;
    fraSynTree.Refresh;  //refresca lo que se tenga del árbol
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
  cxp.DumpCode(edAsm.Lines, Config.IncAddress, Config.IncComment, COnfig.IncVarName );
  edAsm.Lines.Add(';--------------------');
  edAsm.Lines.Add('    END');
  edAsm.EndUpdate;
  //Actualiz panel de árbol de sintaxis
  fraSynTree.Refresh;
end;
procedure TfrmPrincipal.acToolPICExplExecute(Sender: TObject);
begin
  frmPICExplorer.Show;
end;
procedure TfrmPrincipal.acToolConfigExecute(Sender: TObject);
begin
  Config.Mostrar;
end;
//Adicionales
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

