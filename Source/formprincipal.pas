{Programa ejemplo de uso de la librería para implementar editores "utilEditSyn".
                                        Por Tito Hinostroza   11/07/2014 }
unit FormPrincipal;

{$mode objfpc}{$H+}
{$define }
interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, ActnList, StdActns, ExtCtrls, SynFacilUtils,
  SynFacilHighlighter, FormConfig, Parser;

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
    AcHerConfig: TAction;
    AcHerCompil: TAction;
    ActionList: TActionList;
    acVerBarEst: TAction;
    acVerNumLin: TAction;
    acVerPanArc: TAction;
    edAsm: TSynEdit;
    ImageList1: TImageList;
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
    MenuItem2: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnArchivo: TMenuItem;
    mnBuscar: TMenuItem;
    mnEdicion: TMenuItem;
    mnHerram: TMenuItem;
    mnRecientes: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
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
    procedure AcHerCompilExecute(Sender: TObject);
    procedure AcHerConfigExecute(Sender: TObject);
    procedure ChangeEditorState;
    procedure editChangeFileInform;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
  private
    edit: TSynFacilEditor;
    hlAssem : TSynFacilSyn;   //resaltador para ensamblador
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
  SetLanguage('en');
  edit.SetLanguage('en');
  edit := TSynFacilEditor.Create(edPas, 'SinNombre', 'pas');
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
  edit.LoadSyntaxFromFile('Pascal.xml');
  //carga un resaltador a la ventana de ensamblador
  hlAssem := TSynFacilSyn.Create(self);
  edAsm.Highlighter := hlAssem;
  hlAssem.LoadFromFile('asmPic.xml');
end;
procedure TfrmPrincipal.FormShow(Sender: TObject);
begin
  Config.Iniciar(self, edPas);   //necesario para poder trabajar
  edit.InitMenuRecents(mnRecientes, Config.fcEditor.ArcRecientes);  //inicia el menú "Recientes"
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

procedure TfrmPrincipal.editChangeFileInform;
begin
  //actualiza nombre de archivo
  Caption := 'Editor - ' + edit.NomArc;
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
//////////// Acciones de Herramientas ///////////////
procedure TfrmPrincipal.AcHerCompilExecute(Sender: TObject);
{Compila el contenido del archivo actual}
begin
  self.SetFocus;
  cxp.Compilar(edit.NomArc, edPas.Lines);
  edAsm.BeginUpdate(false);
  edAsm.ClearAll;
  edAsm.Text:= ';===RAM usage===' + LineEnding +
               cxp.RAMusage;
  edAsm.Lines.Add(';===Blocks of Code===');
  cxp.DumpCode(edAsm.Lines);
  edAsm.Lines.Add(';===Statistics===');
  cxp.DumpStatistics(edAsm.Lines);
  edAsm.EndUpdate;
  if cxp.HayError then begin
    VerificarError;
//    MsgErr(c.PErr.TxtError);
    exit;
  end;

end;
procedure TfrmPrincipal.AcHerConfigExecute(Sender: TObject);
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
      {If MostrarError Then }cxp.ShowError;
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
  case lowerCase(lang) of
  'es': begin
      //menú principal
      mnArchivo.Caption:='&Archivo';
      mnEdicion.Caption:='&Edición';
      mnBuscar.Caption:='&Buscar';
      mnHerram.Caption:='&Herramientas';

      acArcNuevo.Caption := '&Nuevo';
      acArcNuevo.Hint := 'Nueva consulta';
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
      acHerConfig.Caption:='Configuración';
      acHerConfig.Hint := 'Ver configuración';
    end;
  'en': begin
      //menú principal
      mnArchivo.Caption:='&File';
      mnEdicion.Caption:='&Edit';
      mnBuscar.Caption:='&Search';
      mnHerram.Caption:='&Tools';

      acArcNuevo.Caption := '&New';
      acArcNuevo.Hint := 'New query';
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
      acHerConfig.Caption := '&Settings';
      acHerConfig.Hint := 'Configuration dialog';
    end;
  end;
end;

end.

