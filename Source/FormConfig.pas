{Modelo de formulario de configuración que usa dos Frame de configuración}
unit FormConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, ExtCtrls, ComCtrls, FrameCfgSynEdit, MiConfigXML, MisUtils;
type
  TStyleToolbar = (stb_SmallIcon, stb_BigIcon);

  { TConfig }
  TConfig = class(TForm)
    BitAplicar: TBitBtn;
    BitCancel: TBitBtn;
    BitAceptar: TBitBtn;
    chkIncAddress: TCheckBox;
    chkIncHeadMpu: TCheckBox;
    Edit1: TEdit;
    fcEditor: TfraCfgSynEdit;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RadioGroup1: TRadioGroup;
    tabGeneral: TTabSheet;
    tabEditor: TTabSheet;
    tabEnsamb: TTabSheet;
    procedure BitAceptarClick(Sender: TObject);
    procedure BitAplicarClick(Sender: TObject);
    procedure SetLanguage(lang: string);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FViewPanMsg: boolean;
    FViewStatusbar: Boolean;
    FViewToolbar: boolean;
    procedure cfgFilePropertiesChanges;
    procedure SetViewPanMsg(AValue: boolean);
    procedure SetViewStatusbar(AValue: Boolean);
    procedure SetViewToolbar(AValue: boolean);
  public  //Propiedades generales
    OnPropertiesChanges: procedure of object;
    StateToolbar: TStyleToolbar;
    property ViewStatusbar: Boolean read FViewStatusbar write SetViewStatusbar;
    property ViewToolbar: boolean read FViewToolbar write SetViewToolbar;
    property ViewPanMsg: boolean read FViewPanMsg write SetViewPanMsg;
  public  //Configuraciones para ensamblador
    IncHeadMpu: boolean;  //Incluye encabezado con información del MPU
    IncAddress: boolean;     //Incluye dirección física en el código desensamblado
  public
    procedure Iniciar(ed0, edAsm: TSynEdit);
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
  cfgFile.WindowToProperties;
  if cfgFile.MsjErr<>'' then begin
    MsgErr(cfgFile.MsjErr);
    exit;
  end;
  SaveToFile;
end;
procedure TConfig.Iniciar(ed0, edAsm: TSynEdit);
//Inicia el formulario de configuración. Debe llamarse antes de usar el formulario y
//después de haber cargado todos los frames.
begin
  //Configuraciones generales
  cfgFile.Asoc_Enum('StateStatusbar', @StateToolbar, SizeOf(TStyleToolbar), RadioGroup1, 1);
  cfgFile.Asoc_Bol('VerPanMensaj', @FViewPanMsg  , true);
  cfgFile.Asoc_Bol('VerStatusbar', @ViewStatusbar, true);
  cfgFile.Asoc_Bol('VerBarHerram', @FViewToolbar , true);
  //Configuraciones del Editor
  fcEditor.Iniciar('Edit', cfgFile, ed0);
  //Configuraciones de Ensamblador
  cfgFile.Asoc_Bol('IncHeadMpu', @IncHeadMpu, chkIncHeadMpu, false);
  cfgFile.Asoc_Bol('IncAddress', @IncAddress, chkIncAddress, true);

  cfgFile.OnPropertiesChanges := @cfgFilePropertiesChanges;
  if not cfgFile.FileToProperties then begin
    MsgErr(cfgFile.MsjErr);
  end;
end;
procedure TConfig.FormShow(Sender: TObject);
begin
  if not cfgFile.PropertiesToWindow then begin
    MsgErr(cfgFile.MsjErr);
  end;
end;
procedure TConfig.cfgFilePropertiesChanges;
begin
  fcEditor.ConfigEditor;
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
procedure TConfig.SetLanguage(lang: string);
begin
  fcEditor.SetLanguage(lang);
//  fcEdiAsm.SetLanguage(lang);
  case lowerCase(lang) of
  'es': begin
      Caption := 'Configuración';
      tabGeneral.Caption := 'General';
      tabEditor.Caption := 'Editor';
      chkIncHeadMpu.Caption := 'Incluir Encabezado de MPU';
      chkIncAddress.Caption := 'Incluir Dirección de memoria';
    end;
  'en': begin
      Caption := 'Settings';
      tabGeneral.Caption := 'General';
      tabEditor.Caption := 'Editor';
      chkIncHeadMpu.Caption := 'Include MPU Header';
      chkIncAddress.Caption := 'Include Memory Address';
    end;
  end;
end;

end.

