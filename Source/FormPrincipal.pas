{                                   PicPas.
Compilador en Pascal para micorocntroladores PIC de la serie 16.

                                        Por Tito Hinostroza   22/08/2015 }
unit FormPrincipal;

{$mode objfpc}{$H+}
{$define }
interface

uses
  Classes, SysUtils, types, FileUtil, SynEdit, SynEditMiscClasses, Forms,
  Controls, Graphics, Dialogs, Menus, ComCtrls, ActnList, StdActns, ExtCtrls,
  StdCtrls, LCLIntf, SynFacilUtils, SynFacilHighlighter, MisUtils, FormConfig,
  Parser, FormPICExplorer, Globales, FormCodeExplorer, FrameSyntaxTree,
  FrameCfgIDE;

type

  { TfrmPrincipal }

  TfrmPrincipal = class(TForm)
    acArcAbrir: TAction;
    acArcGuaCom: TAction;
    acArcGuardar: TAction;
    acArcNuevo: TAction;
    acArcSalir: TAction;
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
    acToolConfig: TAction;
    acToolCompil: TAction;
    acToolPICExpl: TAction;
    acToolComEjec: TAction;
    acToolCodExp: TAction;
    acViewToolbar: TAction;
    acViewMsgPan: TAction;
    ActionList: TActionList;
    acViewStatbar: TAction;
    acViewFilePan: TAction;
    edAsm: TSynEdit;
    ImgMessages: TImageList;
    ImgActions32: TImageList;
    ImgActions16: TImageList;
    ImgCompletion: TImageList;
    ListBox1: TListBox;
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
    OpenDialog1: TOpenDialog;
    panMessages: TPanel;
    SaveDialog1: TSaveDialog;
    splSynTree: TSplitter;
    Splitter2: TSplitter;
    splEdPas: TSplitter;
    StatusBar1: TStatusBar;
    edPas: TSynEdit;
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
    procedure acArcAbrirExecute(Sender: TObject);
    procedure acArcGuaComExecute(Sender: TObject);
    procedure acArcGuardarExecute(Sender: TObject);
    procedure acArcNuevoExecute(Sender: TObject);
    procedure acArcSalirExecute(Sender: TObject);
    procedure acEdiRedoExecute(Sender: TObject);
    procedure acEdiSelecAllExecute(Sender: TObject);
    procedure acEdiUndoExecute(Sender: TObject);
    procedure acToolCodExpExecute(Sender: TObject);
    procedure acToolCompilExecute(Sender: TObject);
    procedure acToolConfigExecute(Sender: TObject);
    procedure acToolPICExplExecute(Sender: TObject);
    procedure acViewStatbarExecute(Sender: TObject);
    procedure acViewToolbarExecute(Sender: TObject);
    procedure acViewMsgPanExecute(Sender: TObject);
    procedure ChangeEditorState;
    procedure DoSelectSample(Sender: TObject);
    procedure editChangeFileInform;
    procedure edSpecialLineMarkup(Sender: TObject; Line: integer;
      var Special: boolean; Markup: TSynSelectedColor);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1DrawItem({%H-}Control: TWinControl; Index: Integer;
      ARect: TRect; {%H-}State: TOwnerDrawState);
  private
    edit: TSynFacilEditor;
    hlAssem : TSynFacilSyn;   //resaltador para ensamblador
    fraSynTree: TfraSyntaxTree;  //árbol de sintaxis
    procedure ChangeAppearance;
    procedure MarcarError(nLin, nCol: integer);
    procedure VerificarError;
  public
    procedure SetLanguage(lang: string);
  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

{$R *.lfm}

{ TfrmPrincipal }

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
  fraSynTree := TfraSyntaxTree.Create(self);
  fraSynTree.Parent := self;
  //configura panel de mensajes
  ListBox1.Style:=lbOwnerDrawVariable;
  ListBox1.OnDrawItem:=@ListBox1DrawItem;
  //configuración del editor
  edit := TSynFacilEditor.Create(edPas, 'SinNombre', 'pas');
  edPas.Options:=[eoBracketHighlight];  //quita la línea vertical
  edPas.Options := edPas.Options - [eoSmartTabs];
  edPas.Options := edPas.Options - [eoTrimTrailingSpaces];
  edPas.Options := edPas.Options + [eoKeepCaretX];
  edPas.Options := edPas.Options + [eoTabIndent];  //permite indentar con <Tab>
  edPas.Options2:= edPas.Options2 + [eoCaretSkipTab];
  edPas.TabWidth:= 2;
  edPas.OnSpecialLineMarkup:=@edSpecialLineMarkup;
//  InicEditorC1(edpas);

  edit.OnChangeEditorState:=@ChangeEditorState;
  edit.OnChangeFileInform:=@editChangeFileInform;
  //define paneles
  edit.PanFileSaved := StatusBar1.Panels[0]; //panel para mensaje "Guardado"
  edit.PanCursorPos := StatusBar1.Panels[1];  //panel para la posición del cursor

  edit.PanForEndLin := StatusBar1.Panels[2];  //panel para el tipo de delimitador de línea
  edit.PanCodifFile := StatusBar1.Panels[3];  //panel para la codificación del archivo
  edit.PanLangName  := StatusBar1.Panels[4];  //panel para el lenguaje
  edit.PanFileName  := StatusBar1.Panels[5];  //panel para el nombre del archivo

  edit.NewFile;        //para actualizar estado
  edit.LoadSyntaxFromFile('PicPas_PIC16.xml');
  edit.hl.IconList := ImgCompletion;
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
  splEdPas.Align := alRight;
  edPas.Align := alClient;
  //////////
  SetLanguage('en');
//  SetLanguage('qu');
  edit.SetLanguage('en');
  Config.Iniciar(self, edPas, edAsm);   //necesario para poder trabajar
  Config.fcIDE.OnUpdateChanges := @ChangeAppearance;
  ChangeAppearance;   //primera actualización
  edit.InitMenuRecents(mnRecents, Config.fcEditor.ArcRecientes);  //inicia el menú "Recientes"
  frmCodeExplorer.Init(cxp.TreeElems);  //inicia explorador de código
  //carga archivo de ejemplo
  if FileExists('sample.pas') then edit.LoadFile('sample.pas');
//  if FileExists('SinNombre.pas') then edit.LoadFile('SinNombre.pas');
  //carga lista de ejemplos
  Hay := FindFirst(rutSamples + DirectorySeparator + '*.pas', faAnyFile - faDirectory, SR) = 0;
  while Hay do begin
     //encontró archivo
    AddItemToMenu(mnSamples, '&'+ChangeFileExt(SR.name,''),@DoSelectSample);
    Hay := FindNext(SR) = 0;
  end;
  //Inicia arbol de sintaxis
  fraSynTree.Init(cxp.TreeElems);
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
  //Carga archivo arrastrados
  if edit.SaveQuery then Exit;   //Verifica cambios
  edit.LoadFile(SamFil);
end;

procedure TfrmPrincipal.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if edit.SaveQuery then CanClose := false;   //cancela
end;

procedure TfrmPrincipal.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Config.escribirArchivoIni;  //guarda la configuración actual
end;

procedure TfrmPrincipal.FormDestroy(Sender: TObject);
begin
  hlAssem.Free;
  edit.Free;
end;

procedure TfrmPrincipal.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  txt: String;
begin
  ListBox1.Canvas.FillRect(ARect);  //fondo
  //dibuja ícono
  txt := ListBox1.Items[index];
  if txt='' then exit;
  if txt[1] = '[' then begin
    //ícono de error
    ImgMessages.Draw(ListBox1.Canvas,ARect.Left + 2, ARect.Top + 1, 2);
  end else begin
    ImgMessages.Draw(ListBox1.Canvas,ARect.Left + 2, ARect.Top + 1, 0);
  end;
  //escribe el texto
  ListBox1.Canvas.TextOut (ARect.left + ImgMessages.Width + 8 , ARect.Top + 1, txt);
end;
procedure TfrmPrincipal.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  //Carga archivo arrastrados
  if edit.SaveQuery then Exit;   //Verifica cambios
  edit.LoadFile(FileNames[0]);
  edit.LoadSyntaxFromPath;  //para que busque el archivo apropiado
end;
procedure TfrmPrincipal.ChangeEditorState;
begin
  acArcGuardar.Enabled:=edit.Modified;
  acEdUndo.Enabled:=edit.CanUndo;
  acEdRedo.Enabled:=edit.CanRedo;
  //Para estas acciones no es necesario controlarlas, porque son acciones pre-determinadas
//  acEdiCortar.Enabled  := edit.canCopy;
//  acEdiCopiar.Enabled := edit.canCopy;
//  acEdiPegar.Enabled:= edit.CanPaste;
end;
procedure TfrmPrincipal.ChangeAppearance;
begin
  StatusBar1.Visible := Config.fcIDE.ViewStatusbar;
  acViewStatbar.Checked := Config.fcIDE.ViewStatusbar;
  ToolBar1.Visible := Config.fcIDE.ViewToolbar;
  acViewToolbar.Checked:= Config.fcIDE.ViewToolbar;

  panMessages.Visible:= Config.fcIDE.ViewPanMsg;
  Splitter2.Visible := Config.fcIDE.ViewPanMsg;
  acViewMsgPan.Checked:= Config.fcIDE.ViewPanMsg;

  case Config.fcIDE.StateToolbar of
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
begin
  //actualiza nombre de archivo
  Caption := NOM_PROG + ' - ' + VER_PROG  + ' - ' + edit.NomArc;
end;
procedure TfrmPrincipal.edSpecialLineMarkup(Sender: TObject; Line: integer;
  var Special: boolean; Markup: TSynSelectedColor);
begin
  if Line = edit.linErr then begin
      Special := True ;  //marca como línea especial
      Markup.Background := TColor($3030A0); //color de fondo
  end;
end;

/////////////////// Acciones de Archivo /////////////////////
procedure TfrmPrincipal.acArcNuevoExecute(Sender: TObject);
begin
  edit.NewFile;
end;
procedure TfrmPrincipal.acArcAbrirExecute(Sender: TObject);
begin
  OpenDialog1.Filter:='Pascal files|*.pas|All files|*.*';
  edit.OpenDialog(OpenDialog1);
  Config.escribirArchivoIni;  //para que guarde el nombre del último archivo abierto
end;
procedure TfrmPrincipal.acArcGuardarExecute(Sender: TObject);
begin
  edit.SaveFile;
end;
procedure TfrmPrincipal.acArcGuaComExecute(Sender: TObject);
begin
  edit.SaveAsDialog(SaveDialog1);
end;
procedure TfrmPrincipal.acArcSalirExecute(Sender: TObject);
begin
  frmPrincipal.Close;
end;
//////////// Acciones de Edición ////////////////
procedure TfrmPrincipal.acEdiUndoExecute(Sender: TObject);
begin
  edit.Undo;
end;
procedure TfrmPrincipal.acEdiRedoExecute(Sender: TObject);
begin
  edit.Redo;
end;
procedure TfrmPrincipal.acEdiSelecAllExecute(Sender: TObject);
begin
  edPas.SelectAll;
end;
//////////// Acciones de Ver ///////////////
procedure TfrmPrincipal.acViewStatbarExecute(Sender: TObject);
begin
  Config.fcIDE.ViewStatusbar:=not Config.fcIDE.ViewStatusbar;
end;
procedure TfrmPrincipal.acViewToolbarExecute(Sender: TObject);
begin
  Config.fcIDE.ViewToolbar:= not Config.fcIDE.ViewToolbar;
end;
procedure TfrmPrincipal.acViewMsgPanExecute(Sender: TObject);
begin
  Config.fcIDE.ViewPanMsg:= not Config.fcIDE.ViewPanMsg;
end;

//////////// Acciones de Herramientas ///////////////
procedure TfrmPrincipal.acToolCompilExecute(Sender: TObject);
{Compila el contenido del archivo actual}
var
  timeCnt: DWORD;
begin
  self.SetFocus;
  ListBox1.Items.Clear;
  timeCnt:=GetTickCount;
  ListBox1.Items.Add('Starting Compilation ...');
  cxp.Compile(edit.NomArc, edPas.Lines);
  if cxp.HayError then begin
    ListBox1.Items.Add(cxp.PErr.TxtErrorRC);
    VerificarError;
    exit;
  end;
  ListBox1.Items.Add('Compiled in: ' + IntToStr(GetTickCount-timeCnt) + ' msec');
  //muestra estadísticas
  edAsm.BeginUpdate(false);
  edAsm.ClearAll;
  edAsm.Text:= ';===RAM usage===' + LineEnding +
               cxp.RAMusage;
  edAsm.Lines.Add(';===Blocks of Code===');
  cxp.DumpCode(edAsm.Lines);
//  edAsm.Lines.Add(';===Statistics===');
  cxp.DumpStatistics(ListBox1.Items);
  edAsm.EndUpdate;
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

//ADicionales
procedure TfrmPrincipal.VerificarError;
//Verifica si se ha producido algún error en el preprocesamiento y si lo hay
//Ve la mejor forma de msotrarlo
begin
    If not cxp.HayError Then exit;  //verificación
    //Selecciona posición de error en el Editor
    If cxp.ArcError <> '' Then begin
        //Se ha identificado el archivo con el error
        If edit.NomArc = '' Then begin
            //Tenemos el editor libre para mostrar el archivo
            edit.LoadFile(cxp.ArcError);
            //Ubicamos número de línea, si hay
            MarcarError(cxp.nLinError,cxp.nColError);
            {If MostrarError Then }cxp.ShowError;
        end Else begin
            //Hay un archivo cargado
            If cxp.ArcError = edit.NomArc Then begin
                //El error está en el mismo archivo, lo mostramos
                If cxp.nLinError <> 0 Then begin
                   MarcarError(cxp.nLinError,cxp.nColError);
                   edPas.Invalidate;
                end;
                {If MostrarError Then }cxp.ShowError;
            end Else begin
                //Es otro archivo. Lo abre en otra ventana
//               AbrirPreSQL(cxp.ArcError, cxp.TxtError);
            end;
        end;
    End else begin   //no hay archivo de error
      {If MostrarError Then }
      cxp.ShowError;
    end;
End;
procedure TfrmPrincipal.MarcarError(nLin, nCol: integer);
begin
  //posiciona curosr
  edPas.CaretX := nCol;
  edPas.CaretY := nLin;
  //define línea con error
  edit.linErr := nLin;
  edPas.Invalidate;  //refresca
end;
procedure TfrmPrincipal.SetLanguage(lang: string);
begin
  Config.SetLanguage(lang);
  frmCodeExplorer.SetLanguage('en');
  case lowerCase(lang) of
  'es': begin
      //menú principal
      mnFile.Caption:='&Archivo';
      mnEdit.Caption:='&Edición';
      mnFind.Caption:='&Buscar';
      mnView.Caption:='&Ver';
      mnTools.Caption:='&Herramientas';

      acArcNuevo.Caption := '&Nuevo';
      acArcNuevo.Hint := 'Nuevo archivo';
      acArcAbrir.Caption := '&Abrir...';
      acArcAbrir.Hint := 'Abrir archivo';
      acArcGuardar.Caption := '&Guardar';
      acArcGuardar.Hint := 'Guardar archivo';
      acArcGuaCom.Caption := 'G&uardar Como...';
      acArcGuaCom.Hint := 'Guardar como';
      acArcSalir.Caption := '&Salir';
      acArcSalir.Hint := 'Cerrar el programa';
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
  'en': begin
      //menú principal
      mnFile.Caption:='&File';
      mnEdit.Caption:='&Edit';
      mnFind.Caption:='&Search';
      mnView.Caption:='&View';
      mnTools.Caption:='&Tools';

      acArcNuevo.Caption := '&New';
      acArcNuevo.Hint := 'New File';
      acArcAbrir.Caption := '&Open...';
      acArcAbrir.Hint := 'Open file';
      acArcGuardar.Caption := '&Save';
      acArcGuardar.Hint := 'Save file';
      acArcGuaCom.Caption := 'Sa&ve As ...';
      acArcGuaCom.Hint := 'Save file as ...';
      acArcSalir.Caption := '&Quit';
      acArcSalir.Hint := 'Close the program';
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
  'qu': begin
      //menú principal
      mnFile.Caption:='&Khipu';
      mnEdit.Caption:='&Edición';
      mnFind.Caption:='&Mask''ay';
      mnView.Caption:='&Rikhuy';
      mnTools.Caption:='&Llank''ana';

      acArcNuevo.Caption := '&Nuevo';
      acArcNuevo.Hint := 'Nuevo archivo';
      acArcAbrir.Caption := '&Abrir...';
      acArcAbrir.Hint := 'Abrir archivo';
      acArcGuardar.Caption := '&Guardar';
      acArcGuardar.Hint := 'Guardar archivo';
      acArcGuaCom.Caption := 'G&uardar Como...';
      acArcGuaCom.Hint := 'Guardar como';
      acArcSalir.Caption := '&Salir';
      acArcSalir.Hint := 'Cerrar el programa';
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
  end;
end;

end.

