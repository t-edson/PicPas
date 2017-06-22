{Modelo de formulario de configuración que usa dos Frame de configuración}
unit FormConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, ExtCtrls, ComCtrls, ColorBox, FrameCfgSynEdit, Globales,
  FrameCfgSyntax, MiConfigXML, MisUtils;
type
  //Tipo de Barra de herramientas
  TStyleToolbar = (stb_SmallIcon, stb_BigIcon);
  //Tipo de declaración de variables
  TVarDecType = (dvtDBDb,  //Estilo DB/Db/DW
                 dvtEQU    //Estilo usando macros y EQU
                 );
  //Niveles de optimización
  TOptimLev = (olvFool,   //Nivel básico de optimización
               olvSmart   //Nivel mayor de optimización
               );
  TTreeViewMode = (vmGroups,   //Muestra por grupos
                   vmDeclar,   //Muestra en el orden de declaración
                   vmFileExp   //Muestra el explorador de archivos
                   );
  { TConfig }
  TConfig = class(TForm)
    BitAplicar: TBitBtn;
    BitCancel: TBitBtn;
    BitAceptar: TBitBtn;
    butDefval: TButton;
    chkIncVarName: TCheckBox;
    chkLoadLast: TCheckBox;
    chkSetProIniBnk: TCheckBox;
    chkSetProEndBnk: TCheckBox;
    chkShowErrMsg: TCheckBox;
    chkIncComment2: TCheckBox;
    chkExcUnused: TCheckBox;
    chkIncDecVar: TCheckBox;
    chkIncAddress: TCheckBox;
    chkIncComment: TCheckBox;
    chkIncHeadMpu: TCheckBox;
    colCodExplBack: TColorBox;
    colMessPanPan: TColorBox;
    colSplitterCol: TColorBox;
    colMessPanSel: TColorBox;
    colMessPanText: TColorBox;
    colMessPanBack: TColorBox;
    colCodExplText: TColorBox;
    colMessPanErr: TColorBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    grpTabEdiState: TRadioGroup;
    Label1: TLabel;
    Label2: TLabel;
    lblCodExplCol1: TLabel;
    lblCodExplCol2: TLabel;
    lblMessPan1: TLabel;
    lblMessPan2: TLabel;
    lblMessPan3: TLabel;
    lblMessPan4: TLabel;
    lblPanelCol: TLabel;
    lblSplitterCol: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RadioGroup1: TRadioGroup;
    grpOptimLev: TRadioGroup;
    RadioGroup2: TRadioGroup;
    tabGeneral: TTabSheet;
    tabEditor: TTabSheet;
    tabEnsamb: TTabSheet;
    tabOutput: TTabSheet;
    tabEnviron: TTabSheet;
    tabSyntax: TTabSheet;
    procedure BitAceptarClick(Sender: TObject);
    procedure BitAplicarClick(Sender: TObject);
    procedure butDefvalClick(Sender: TObject);
    procedure chkIncDecVarChange(Sender: TObject);
    procedure SetLanguage(idLang: string);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FViewPanMsg: boolean;
    FViewStatusbar: Boolean;
    FViewSynTree: boolean;
    FViewToolbar: boolean;
    procedure cfgFilePropertiesChanges;
    procedure SetViewPanMsg(AValue: boolean);
    procedure SetViewStatusbar(AValue: Boolean);
    procedure SetViewSynTree(AValue: boolean);
    procedure SetViewToolbar(AValue: boolean);
  public  //Configuraciones generales
    StateToolbar: TStyleToolbar;
    SynTreeWidth: integer;   //Ancho del panel del árbol ed sintaxis
    viewMode  : TTreeViewMode;
    language : string;
    TabEdiMode: integer;  //Estado de pestañas del editor
    property ViewStatusbar: Boolean read FViewStatusbar write SetViewStatusbar;
    property ViewToolbar: boolean read FViewToolbar write SetViewToolbar;
    property ViewPanMsg: boolean read FViewPanMsg write SetViewPanMsg;
    property ViewSynTree: boolean read FViewSynTree write SetViewSynTree;
  public  //Configuraciones de entorno
    CodExplBack: TColor;
    CodExplText: TColor;
    MessPanBack: TColor;  //Color de fondo del panel de mensajes
    MessPanText: TColor;  //Color del texto del panel de mensajes
    MessPanErr : TColor;  //Color del texto de error del panel de mensajes
    MessPanSel : TColor;  //Color del fonde de la selección del panel de mensajes
    PanelsCol : TColor;  //Color de los panels del Panel de Mensages
    SplitterCol: TColor;  //Color de separadores
    LoadLast   : boolean;  //Cargar el último archivo editado
  public  //Configuraciones para ensamblador
    IncHeadMpu: boolean;  //Incluye encabezado con información del MPU
    IncVarDec : boolean;  //Incluye declaración de varaibles
    VarDecType: TVarDecType;  //tipo de declaración de variables
    IncAddress: boolean;  //Incluye dirección física en el código desensamblado
    IncComment: boolean;  //Incluye comentarios en el código desensamblado
    IncComment2: boolean; //Incluye comentarios detallados en el código desensamblado
    ExcUnused : boolean;  //Excluye declaración de variables no usadas
    IncVarName: boolean;  //Reemplaza dirección con etiqueta de variables
    //Configuracions del compilador
    ShowErMsg : boolean;
    OptimLev  : TOptimLev;
    SetProIniBnk: Boolean;
    SetProEndBnk: Boolean;
    procedure ConfigEditor(ed: TSynEdit);
  public
    fraCfgSynEdit: TfraCfgSynEdit;
    fraCfgSyntax : TfraCfgSyntax;
    OnPropertiesChanges: procedure of object;
    procedure Iniciar;
    procedure Mostrar;
    procedure SaveToFile;
  end;

var
  Config: TConfig;

implementation
{$R *.lfm}
{ TConfig }
procedure TConfig.FormCreate(Sender: TObject);
begin
  fraCfgSynEdit := TfraCfgSynEdit.Create(self);
  fraCfgSynEdit.Parent := tabEditor;
  fraCfgSynEdit.Left := 20;
  fraCfgSynEdit.Top := 5;

  fraCfgSyntax := TfraCfgSyntax.Create(self);
  fraCfgSyntax.Parent := tabSyntax;
  fraCfgSyntax.Left := 10;
  fraCfgSyntax.Top := 5;

  cfgFile.VerifyFile;
end;
procedure TConfig.BitAceptarClick(Sender: TObject);
begin
  bitAplicarClick(Self);
  if cfgFile.MsjErr<>'' then exit;  //hubo error
  self.Close;  //sale si no hay error
end;
procedure TConfig.BitAplicarClick(Sender: TObject);
begin
  //Guarda primero, para tener actualziado los archivos de sintaxis, cuando se dispare "OnPropertiesChanges"
  fraCfgSyntax.SaveChanges;
  //Proceso normal
  cfgFile.WindowToProperties;
  if cfgFile.MsjErr<>'' then begin
    MsgErr(cfgFile.MsjErr);
    exit;
  end;
  SaveToFile;
end;

procedure TConfig.butDefvalClick(Sender: TObject);
begin
  CodExplBack := clWindow;
  CodExplText := clDefault;
  MessPanBack := clWindow;

  MessPanText :=clDefault;
  MessPanErr  :=clRed;
  MessPanSel  :=clBtnFace;
  PanelsCol   :=clDefault;

  SplitterCol := clDefault;
  LoadLast    := true;
  cfgFile.PropertiesToWindow;
end;

procedure TConfig.chkIncDecVarChange(Sender: TObject);
begin
  RadioGroup2.Enabled := chkIncDecVar.Checked;
  chkExcUnused.Enabled := chkIncDecVar.Checked;
end;
procedure TConfig.Iniciar;
//Inicia el formulario de configuración. Debe llamarse antes de usar el formulario y
//después de haber cargado todos los frames.
begin
  //Configuraciones generales
  cfgFile.Asoc_Enum('StateStatusbar', @StateToolbar, SizeOf(TStyleToolbar), RadioGroup1, 1);
  cfgFile.Asoc_Bol('VerPanMensaj', @FViewPanMsg  , true);
  cfgFile.Asoc_Bol('VerStatusbar', @ViewStatusbar, true);
  cfgFile.Asoc_Bol('VerBarHerram', @FViewToolbar , true);
  cfgFile.Asoc_Bol('ViewSynTree',  @FViewSynTree, true);
  cfgFile.Asoc_Int('SynTreeWidth', @SynTreeWidth, 130);
  cfgFile.Asoc_Str('language'   , @language, ComboBox1, 'en - English');
  cfgFile.Asoc_Int('TabEdiState', @TabEdiMode, grpTabEdiState, 0);
  //Configuraciones de entorno
  cfgFile.Asoc_TCol('CodExplBack',@CodExplBack, colCodExplBack, clWindow);
  cfgFile.Asoc_TCol('CodExplText',@CodExplText, colCodExplText, clDefault);

  cfgFile.Asoc_TCol('MessPanBack',@MessPanBack, colMessPanBack, clWindow);
  cfgFile.Asoc_TCol('MessPanText',@MessPanText, colMessPanText, clDefault);
  cfgFile.Asoc_TCol('MessPanErr', @MessPanErr , colMessPanErr , clRed);
  cfgFile.Asoc_TCol('MessPanSel', @MessPanSel , colMessPanSel , clBtnFace);
  cfgFile.Asoc_TCol('MessPanPan', @PanelsCol , colMessPanPan , clDefault);

  cfgFile.Asoc_TCol('SplitterCol',@SplitterCol, colSplitterCol, clDefault);
  cfgFile.Asoc_Bol ('chkLoadLast',@LoadLast   , chkLoadLast   , true);
  //Configuraciones del Editor
  fraCfgSynEdit.Iniciar('Edit', cfgFile);
  //COnfigruación de sintaxis
  fraCfgSyntax.Init(rutSyntax);
  //COnfiguración de Vista
  cfgFile.Asoc_Enum('viewMode',  @viewMode   , SizeOf(TTreeViewMode), 0);
  //Configuraciones de Ensamblador
  cfgFile.Asoc_Bol('IncHeadMpu', @IncHeadMpu , chkIncHeadMpu , false);
  cfgFile.Asoc_Bol('IncDecVar' , @IncVarDec  , chkIncDecVar  , true);
  cfgFile.Asoc_Enum('VarDecType',@VarDecType , Sizeof(TVarDecType), RadioGroup2, 1);
  cfgFile.Asoc_Bol('IncAddress', @IncAddress , chkIncAddress , true);
  cfgFile.Asoc_Bol('IncComment', @IncComment , chkIncComment , false);
  cfgFile.Asoc_Bol('IncComment2',@IncComment2, chkIncComment2, false);
  cfgFile.Asoc_Bol('ExcUnused' , @ExcUnused  , chkExcUnused  , false);
  cfgFile.Asoc_Bol('IncVarName', @IncVarName , chkIncVarName , true);
  //Configuraciones del compilador
  cfgFile.Asoc_Bol('ShowErMsg' , @ShowErMsg, chkShowErrMsg, true);
  cfgFile.Asoc_Enum('OptimLev' , @OptimLev, Sizeof(TOptimLev), grpOptimLev, 1);
  cfgFile.Asoc_Bol('SetProIniBnk', @SetProIniBnk, chkSetProIniBnk, true);
  cfgFile.Asoc_Bol('SetProEndBnk', @SetProEndBnk, chkSetProEndBnk, true);
  //////////////////////////////////////////////////
  cfgFile.OnPropertiesChanges := @cfgFilePropertiesChanges;
  if not cfgFile.FileToProperties then begin
    MsgErr(cfgFile.MsjErr);
  end;
  chkIncDecVarChange(self);   //para actualizar
end;
procedure TConfig.FormShow(Sender: TObject);
begin
  if not cfgFile.PropertiesToWindow then begin
    MsgErr(cfgFile.MsjErr);
  end;
end;
procedure TConfig.ConfigEditor(ed: TSynEdit);
//Configura un editor con las opciones definidas aquí
begin
  fraCfgSynEdit.ConfigEditor(ed);
end;
procedure TConfig.cfgFilePropertiesChanges;
begin
  if OnPropertiesChanges<>nil then OnPropertiesChanges;
end;
procedure TConfig.SetViewPanMsg(AValue: boolean);
begin
  if FViewPanMsg = AValue then Exit;
  FViewPanMsg := AValue;
  cfgFilePropertiesChanges;
end;
procedure TConfig.SetViewStatusbar(AValue: Boolean);
begin
  if FViewStatusbar = AValue then Exit;
  FViewStatusbar := AValue;
  cfgFilePropertiesChanges;
end;

procedure TConfig.SetViewSynTree(AValue: boolean);
begin
  if FViewSynTree = AValue then Exit;
  FViewSynTree := AValue;
  cfgFilePropertiesChanges;
end;

procedure TConfig.SetViewToolbar(AValue: boolean);
begin
  if FViewToolbar = AValue then Exit;
  FViewToolbar := AValue;
  cfgFilePropertiesChanges;
end;
procedure TConfig.Mostrar;
//Muestra el formulario para configurarlo
begin
  Showmodal;
end;
procedure TConfig.SaveToFile;
begin
  if not cfgFile.PropertiesToFile then begin
    MsgErr(cfgFile.MsjErr);
  end;
end;
procedure TConfig.SetLanguage(idLang: string);
begin
  fraCfgSynEdit.SetLanguage(idLang);
  curLang := idLang;
  {$I ..\language\tra_FormConfig.pas}
end;

end.

