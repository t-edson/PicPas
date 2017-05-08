{                                   PicPas.
Compilador en Pascal para micorocntroladores PIC de la serie 16.

                                        Por Tito Hinostroza   22/08/2015 }
unit FormPrincipal;
{$mode objfpc}{$H+}
{$define }
interface
uses
  Classes, SysUtils, types, SynEdit, Forms, Controls, Dialogs, Menus, ComCtrls,
  ActnList, StdActns, ExtCtrls, LCLIntf, LCLType, SynFacilHighlighter,
  SynFacilUtils, MisUtils, XpresBas, Parser, FormPICExplorer, Globales,
  FormCodeExplorer, FrameSyntaxTree, FormConfig, PicPasProject, FrameEditView,
  FrameMessagesWin;
type
  { TfrmPrincipal }
  TfrmPrincipal = class(TForm)
    acArcOpen: TAction;
    acArcSaveAs: TAction;
    acArcSave: TAction;
    acArcNewFile: TAction;
    acArcQuit: TAction;
    acBusBuscar: TAction;
    acBusBusSig: TAction;
    acBusReemp: TAction;
    acEdCopy: TEditCopy;
    acEdCut: TEditCut;
    acEdModCol: TAction;
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
    acToolCodExp: TAction;
    acViewToolbar: TAction;
    acViewMsgPan: TAction;
    ActionList: TActionList;
    acViewStatbar: TAction;
    acViewSynTree: TAction;
    edAsm: TSynEdit;
    fraEditView1: TfraEditView;
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
    splSynTree: TSplitter;
    Splitter2: TSplitter;
    splEdPas: TSplitter;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
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
    procedure acToolCodExpExecute(Sender: TObject);
    procedure acToolCompilExecute(Sender: TObject);
    procedure acToolConfigExecute(Sender: TObject);
    procedure acToolPICExplExecute(Sender: TObject);
    procedure acViewSynTreeExecute(Sender: TObject);
    procedure acViewStatbarExecute(Sender: TObject);
    procedure acViewToolbarExecute(Sender: TObject);
    procedure acViewMsgPanExecute(Sender: TObject);
    procedure ChangeEditorState(ed: TSynEditor);
    procedure DoSelectSample(Sender: TObject);
    procedure editChangeFileInform;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    curProj: TPicPasProject;  //Proyecto actual
    hlAssem : TSynFacilSyn;   //resaltador para ensamblador
    fraSynTree : TfraSyntaxTree;  //árbol de sintaxis
    fraMessages: TfraMessagesWin;
    procedure ChangeAppearance;
    procedure fraEditView1SelectEditor;
    procedure fraSynTreeSelectElemen(out srcPos: TSrcPos);
    procedure MarcarError(ed: TSynEditor; nLin, nCol: integer);
    procedure VerificarError;
  public
    procedure SetLanguage(lang: string);
  end;

var
  frmPrincipal: TfrmPrincipal;

implementation
{$R *.lfm}
{ TfrmPrincipal }
procedure TfrmPrincipal.fraSynTreeSelectElemen(out srcPos: TSrcPos);
begin
//  fraEditView1.SelectOrLoad()
end;
procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
  fraSynTree := TfraSyntaxTree.Create(self);
  fraSynTree.Parent := self;
  fraMessages := TfraMessagesWin.Create(self);
  fraMessages.Parent := panMessages;  //Ubica
  fraMessages.Align := alClient;
  //configura panel de mensajes
  fraEditView1.OnChangeEditorState := @ChangeEditorState;
  fraEditView1.OnSelectEditor := @fraEditView1SelectEditor;
  //COnfigura Árbol de sintaxis
  fraSynTree.OnSelectElemen := @fraSynTreeSelectElemen;
  //carga un resaltador a la ventana de ensamblador
  hlAssem := TSynFacilSyn.Create(self);
  edAsm.Highlighter := hlAssem;
  hlAssem.LoadFromFile('PicPas_AsmPic.xml');
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
  //////////
  SetLanguage('en');
  fraEditView1.tmpPath := rutTemp;   //fija ruta de trabajo
  fraEditView1.SetLanguage('en');
//  SetLanguage('qu');
  Config.Iniciar;   //necesario para poder trabajar
  Config.OnPropertiesChanges := @ChangeAppearance;
  ChangeAppearance;   //primera actualización
//  edit.InitMenuRecents(mnRecents, Config.fcEditor.ArcRecientes);  //inicia el menú "Recientes"
  frmCodeExplorer.Init(cxp.TreeElems);  //inicia explorador de código
  //carga archivo de ejemplo
//  if FileExists('sample.pas') then fraEditView1.AddEdit('sample.pas');
  if FileExists('SinNombre.pas') then fraEditView1.LoadFile('SinNombre.pas');
  //carga lista de ejemplos
  Hay := FindFirst(rutSamples + DirectorySeparator + '*.pas', faAnyFile - faDirectory, SR) = 0;
  while Hay do begin
     //encontró archivo
    AddItemToMenu(mnSamples, '&'+ChangeFileExt(SR.name,''),@DoSelectSample);
    Hay := FindNext(SR) = 0;
  end;
  //Inicia arbol de sintaxis
  fraSynTree.Init(cxp.TreeElems);
  //Inicia encabezado
  editChangeFileInform;
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
  Config.SaveToFile;  //guarda la configuración actual
end;
procedure TfrmPrincipal.FormDestroy(Sender: TObject);
begin
  hlAssem.Free;
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
    if fraEditView1.HasFocus then fraEditView1.SelectNextEditor;
  end;
  if (Shift = [ssShift, ssCtrl]) and (Key = VK_TAB) then begin
    if fraEditView1.HasFocus then fraEditView1.SelectPrevEditor;
  end;
  if (Shift = [ssCtrl]) and (Key = VK_F4) then begin
    if fraEditView1.HasFocus then acArcCloseFileExecute(self);
    Shift := []; Key := 0;  //para qie no pase
  end;
end;
procedure TfrmPrincipal.ChangeEditorState(ed: TSynEditor);
begin
  acArcSave.Enabled:=ed.Modified;
  acEdUndo.Enabled:=ed.CanUndo;
  acEdRedo.Enabled:=ed.CanRedo;
  //Para estas acciones no es necesario controlarlas, porque son acciones pre-determinadas
//  acEdiCortar.Enabled  := edit.canCopy;
//  acEdiCopiar.Enabled := edit.canCopy;
//  acEdiPegar.Enabled:= edit.CanPaste;
end;
procedure TfrmPrincipal.fraEditView1SelectEditor;
begin
  ChangeAppearance;
  editChangeFileInform;
end;
procedure TfrmPrincipal.ChangeAppearance;
  procedure SetStateActionsProject(state: boolean);
  begin
    acArcSave.Enabled := state;
    acArcCloseProj.Enabled := state;
    acEdUndo.Enabled := state;
    acEdRedo.Enabled := state;
    acEdCut.Enabled := state;
    acEdCopy.Enabled := state;
    acEdPaste.Enabled := state;
    acEdSelecAll.Enabled := state;
  end;
begin
  if curProj = nil then begin
    SetStateActionsProject(false);
//    exit;
  end else begin
    SetStateActionsProject(true);
  end;
  if fraEditView1.Count = 0 then begin
    //No hay ventanas de edición abiertas
    fraEditView1.Visible := false;
    acArcSaveAs.Enabled := false;
  end else begin
    //Hay ventanas de edición abiertas
    fraEditView1.Visible := true;
    acArcSaveAs.Enabled := true;
  end;

  fraSynTree.Visible := Config.ViewSynTree;
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
end;
procedure TfrmPrincipal.editChangeFileInform;
{Actualiza la barra de título, de acuerdo al estado}
var
  ed: TSynEditor;
begin
  if curProj= nil then begin
    //Modo de archivos. Actualiza nombre de archivo
    if fraEditView1.Count = 0 then begin
      Caption := NOM_PROG + ' - ' + VER_PROG  + ' - No files.';
    end else begin  //Hay varios
      ed := fraEditView1.ActiveEditor;
      if ed.NomArc='' then
        Caption := NOM_PROG + ' - ' + VER_PROG  + ' - ' + ed.Caption
      else
        Caption := NOM_PROG + ' - ' + VER_PROG  + ' - ' + ed.NomArc;
    end;
  end else begin
    //Hay un proyecto abierto
    Caption := NOM_PROG + ' - ' + VER_PROG  + ' - Project: ' + curProj.name;
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
  fraEditView1.OpenDialog('Pascal files|*.pas|All files|*.*');
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
  Config.ViewSynTree := not config.ViewSynTree;
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
  cxp.Compile(filName, fraEditView1.ActiveEditor.SynEdit.Lines);
  if cxp.HayError then begin
    fraMessages.EndCompilation;
    VerificarError;
    exit;
  end;
  fraMessages.EndCompilation;
  //Genera código ensamblador
  edAsm.BeginUpdate(false);
  edAsm.Lines.Clear;
  if Config.IncHeadMpu then begin
    //Incluye encabezado
     edAsm.Lines.Add('    list     p='+ cxp.PicName);
     edAsm.Lines.Add('    #include <' + cxp.PicName + '.inc>');
//     edAsm.Lines.Add('    __CONFIG        _CP_OFF & _PWRTE_ON & _WDT_OFF & _XT_OSC');
  end;
  if Config.IncVarDec then begin
     edAsm.Lines.Add(';===RAM usage===');
     cxp.RAMusage(edAsm.Lines, Config.VarDecType);
  end;
  edAsm.Lines.Add(';===Blocks of Code===');
  cxp.DumpCode(edAsm.Lines, Config.IncAddress, Config.IncComment);
  edAsm.EndUpdate;
  //Actualiz panel de árbol de sintaxis
  fraSynTree.Refresh;
end;
procedure TfrmPrincipal.acToolPICExplExecute(Sender: TObject);
begin
  frmPICExplorer.Show;
end;
procedure TfrmPrincipal.acToolCodExpExecute(Sender: TObject);
begin
  frmCodeExplorer.Refresh;
  frmCodeExplorer.Show;
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
        {If MostrarError Then }MsgErr(msg);
    end else begin   //no hay archivo de error
      {If MostrarError Then } MsgErr(msg);
    end;
End;
procedure TfrmPrincipal.MarcarError(ed: TSynEditor; nLin, nCol: integer);
begin
  fraEditView1.SetFocus;
  //posiciona curosr
  ed.SynEdit.CaretX := nCol;
  ed.SynEdit.CaretY := nLin;
  //Define línea con error
  ed.linErr := nLin;
  ed.SynEdit.Invalidate;  //refresca
end;
procedure TfrmPrincipal.SetLanguage(lang: string);
begin
  Config.SetLanguage(lang);
  frmCodeExplorer.SetLanguage(lang);
  case lowerCase(lang) of
  'es': begin
      //menú principal
      mnFile.Caption:='&Archivo';
      mnEdit.Caption:='&Edición';
      mnFind.Caption:='&Buscar';
      mnView.Caption:='&Ver';
      mnTools.Caption:='&Herramientas';

      acArcNewFile.Caption := 'Nuevo &Archivo';
      acArcNewFile.Hint := 'Nuevo Archivo';
      acArcNewProj.Caption := 'Nuevo &Proyecto';
      acArcNewProj.Hint := 'Nuevo Proyecto';
      acArcOpen.Caption := '&Abrir...';
      acArcOpen.Hint := 'Abrir archivo';
      acArcSave.Caption := '&Guardar';
      acArcSave.Hint := 'Guardar archivo';
      acArcSaveAs.Caption := 'G&uardar Como...';
      acArcSaveAs.Hint := 'Guardar como';
      acArcQuit.Caption := '&Salir';
      acArcQuit.Hint := 'Cerrar el programa';
      acEdUndo.Caption := '&Deshacer';
      acEdUndo.Hint := 'Deshacer';
      acEdRedo.Caption := '&Rehacer';
      acEdRedo.Hint := 'Reahacer';
      acEdCut.Caption := 'Cor&tar';
      acEdCut.Hint := 'Cortar';
      acEdCopy.Caption := '&Copiar';
      acEdCopy.Hint := 'Copiar';
      acEdPaste.Caption := '&Pegar';
      acEdPaste.Hint := 'Pegar';
      acEdSelecAll.Caption := 'Seleccionar &Todo';
      acEdSelecAll.Hint := 'Seleccionar todo';
      acEdModCol.Caption := 'Modo Columna';
      acEdModCol.Hint := 'Modo columna';
      acBusBuscar.Caption := 'Buscar...';
      acBusBuscar.Hint := 'Buscar texto';
      acBusBusSig.Caption := 'Buscar &Siguiente';
      acBusBusSig.Hint := 'Buscar Siguiente';
      acBusReemp.Caption := '&Remplazar...';
      acBusReemp.Hint := 'Reemplazar texto';

      acViewMsgPan.Caption:='Panel de &Mensajes';
      acViewMsgPan.Hint:='Mostrar u Ocultar el Panel de Mensajes';
      acViewStatbar.Caption:='Barra de &Estado';
      acViewStatbar.Hint:='Mostrar u Ocultar la barra de estado';
      acViewToolbar.Caption:='Barra de &Herramientas';
      acViewToolbar.Hint:='Mostrar u Ocultar la barra de herramientas';

      acToolCompil.Caption:='&Compilar';
      acToolCompil.Hint:='Compila el código fuente';
      acToolComEjec.Caption:='Compilar y Ej&ecutar';
      acToolComEjec.Hint:='Compilar y Ejecutar';
      acToolPICExpl.Caption:='E&xplorador de PIC';
      acToolPICExpl.Hint:='Abre el explorador de dispositivos PIC';
      acToolCodExp.Caption:='Explorador de Co&digo';
      acToolCodExp.Hint:='Abre el explorador de Código';
      acToolConfig.Caption:='Configuración';
      acToolConfig.Hint := 'Ver configuración';
    end;
  'qu': begin
      //menú principal
      mnFile.Caption:='&Khipu';
      mnEdit.Caption:='&Edición';
      mnFind.Caption:='&Mask''ay';
      mnView.Caption:='&Rikhuy';
      mnTools.Caption:='&Llank''ana';

      acArcNewFile.Caption := '&Nuevo';
      acArcNewFile.Hint := 'Nuevo archivo';
      acArcOpen.Caption := '&Abrir...';
      acArcOpen.Hint := 'Abrir archivo';
      acArcSave.Caption := '&Guardar';
      acArcSave.Hint := 'Guardar archivo';
      acArcSaveAs.Caption := 'G&uardar Como...';
      acArcSaveAs.Hint := 'Guardar como';
      acArcQuit.Caption := '&Salir';
      acArcQuit.Hint := 'Cerrar el programa';
      acEdUndo.Caption := '&Deshacer';
      acEdUndo.Hint := 'Deshacer';
      acEdRedo.Caption := '&Rehacer';
      acEdRedo.Hint := 'Reahacer';
      acEdCut.Caption := 'Cor&tar';
      acEdCut.Hint := 'Cortar';
      acEdCopy.Caption := '&Copiar';
      acEdCopy.Hint := 'Copiar';
      acEdPaste.Caption := '&Pegar';
      acEdPaste.Hint := 'Pegar';
      acEdSelecAll.Caption := 'Seleccionar &Todo';
      acEdSelecAll.Hint := 'Seleccionar todo';
      acEdModCol.Caption := 'Modo Columna';
      acEdModCol.Hint := 'Modo columna';
      acBusBuscar.Caption := 'Buscar...';
      acBusBuscar.Hint := 'Buscar texto';
      acBusBusSig.Caption := 'Buscar &Siguiente';
      acBusBusSig.Hint := 'Buscar Siguiente';
      acBusReemp.Caption := '&Remplazar...';
      acBusReemp.Hint := 'Reemplazar texto';

      acViewMsgPan.Caption:='Panel de &Mensajes';
      acViewMsgPan.Hint:='Mostrar u Ocultar el Panel de Mensajes';
      acViewStatbar.Caption:='Barra de &Estado';
      acViewStatbar.Hint:='Mostrar u Ocultar la barra de estado';
      acViewToolbar.Caption:='Barra de &Herramientas';
      acViewToolbar.Hint:='Mostrar u Ocultar la barra de herramientas';

      acToolCompil.Caption:='&Compilar';
      acToolCompil.Hint:='Compila el código fuente';
      acToolComEjec.Caption:='Compilar y Ej&ecutar';
      acToolComEjec.Hint:='Compilar y Ejecutar';
      acToolPICExpl.Caption:='E&xplorador de PIC';
      acToolPICExpl.Hint:='Abre el explorador de dispositivos PIC';
      acToolCodExp.Caption:='Explorador de Co&digo';
      acToolCodExp.Hint:='Abre el explorador de Código';
      acToolConfig.Caption:='Configuración';
      acToolConfig.Hint := 'Ver configuración';
    end;
  else begin  //Inglés o idioma desconocido
      //menú principal
      mnFile.Caption:='&File';
      mnEdit.Caption:='&Edit';
      mnFind.Caption:='&Search';
      mnView.Caption:='&View';
      mnTools.Caption:='&Tools';

      acArcNewFile.Caption := 'New &File';
      acArcNewFile.Hint := 'New File';
      acArcNewProj.Caption := 'New &Project';
      acArcNewProj.Hint := 'New &Project';
      acArcOpen.Caption := '&Open...';
      acArcOpen.Hint := 'Open file';
      acArcSave.Caption := '&Save';
      acArcSave.Hint := 'Save file';
      acArcSaveAs.Caption := 'Sa&ve As ...';
      acArcSaveAs.Hint := 'Save file as ...';
      acArcQuit.Caption := '&Quit';
      acArcQuit.Hint := 'Close the program';
      acEdUndo.Caption := '&Undo';
      acEdUndo.Hint := 'Undo';
      acEdRedo.Caption := '&Redo';
      acEdRedo.Hint := 'Redo';
      acEdCut.Caption := 'C&ut';
      acEdCut.Hint := 'Cut';
      acEdCopy.Caption := '&Copy';
      acEdCopy.Hint := 'Copy';
      acEdPaste.Caption := '&Paste';
      acEdPaste.Hint := 'Paste';
      acEdSelecAll.Caption := 'Select &All';
      acEdSelecAll.Hint := 'Select all';
      acEdModCol.Caption := 'Column mode';
      acEdModCol.Hint := 'Column mode';
      acBusBuscar.Caption := 'Search...';
      acBusBuscar.Hint := 'Search text';
      acBusBusSig.Caption := 'Search &Next';
      acBusBusSig.Hint := 'Search Next';
      acBusReemp.Caption := '&Replace...';
      acBusReemp.Hint := 'Replace text';

      acViewMsgPan.Caption:='&Messages Panel';
      acViewMsgPan.Hint:='Show o hide the Messages Panel';
      acViewStatbar.Caption:='&Status Bar';
      acViewStatbar.Hint:='Show o hide the Status Bar';
      acViewToolbar.Caption:='&Tool Bar';
      acViewToolbar.Hint:='Show o hide the Tool Bar';

      acToolCompil.Caption:='&Compile';
      acToolCompil.Hint:='Compile the source code';
      acToolComEjec.Caption:='Compile and E&xecute';
      acToolComEjec.Hint:='Compile and Execute';
      acToolPICExpl.Caption:='PIC E&xplorer';
      acToolPICExpl.Hint:='Open the PIC devices explorer';
      acToolCodExp.Caption:='Co&de Explorer';
      acToolCodExp.Hint:='Open the Code Explorer';
      acToolConfig.Caption := '&Settings';
      acToolConfig.Hint := 'Settings dialog';
    end;
  end;
end;

end.
//777
