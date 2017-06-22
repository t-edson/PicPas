{Unidad con frame para almacenar y configurar las propiedades de un editor
 SynEdit. Las propiedades que se manejan son con respecto al coloreado.
 El frame definido, está pensado para usarse en una ventana de configuración.
 También incluye una lista para almacenamiento de los archivos recientes
                               Por Tito Hinostroza  23/11/2013
}
unit FrameCfgSynEdit;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Forms, StdCtrls, Dialogs, Spin, SynEdit, Graphics,
  Globales, MiConfigXML, SynEditMarkupHighAll, SynEditMarkup;
type

  { TfraCfgSynEdit }
  TfraCfgSynEdit = class(TFrame)
    cbutFonPan: TColorButton;
    cbutResPal: TColorButton;
    cbutTxtPan: TColorButton;
    chkHighCurWord: TCheckBox;
    chkViewHScroll: TCheckBox;
    chkAutoindent: TCheckBox;
    chkVerPanVer: TCheckBox;
    chkHighCurLin: TCheckBox;
    cbutLinAct: TColorButton;
    chkViewVScroll: TCheckBox;
    chkVerNumLin: TCheckBox;
    chkVerMarPle: TCheckBox;
    cmbFontName: TComboBox;
    cbutBackCol: TColorButton;
    cbutTextCol: TColorButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    spFontSize: TSpinEdit;
    procedure chkHighCurLinChange(Sender: TObject);
    procedure chkHighCurWordChange(Sender: TObject);
    procedure chkVerPanVerChange(Sender: TObject);
  public
    //configuración del editor
    TipLet     : string;    //tipo de letra
    TamLet     : integer;   //tamaño de letra
    MarLinAct  : boolean;   //marcar línea actual
    VerBarDesV : boolean;   //ver barras de desplazamiento
    VerBarDesH : boolean;   //ver barras de desplazamiento
    Autoindent : boolean;   //Autotabulación
    ResPalCur  : boolean;   //resaltar palabra bajo el cursor
    cTxtNor    : TColor;    //color de texto normal
    cFonEdi    : TColor;    //Color de fondo del control de edición
    cFonSel    : TColor;    //color del fondo de la selección
    cTxtSel    : TColor;    //color del texto de la selección
    cLinAct    : TColor;    //color de la línea actual
    cResPal    : TColor;    //color de la palabra actual
    //panel vertical
    VerPanVer  : boolean;    //ver pánel vertical
    VerNumLin  : boolean;    //ver número de línea
    VerMarPle  : boolean;    //ver marcas de plegado
    cFonPan     : TColor;    //color de fondo del panel vertical
    cTxtPan     : TColor;    //color de texto del panel vertical
    ArcRecientes: TStringList;  //Lista de archivos recientes

    procedure PropToWindow;
    procedure Iniciar(section: string; cfgFile: TMiConfigXML); //Inicia el frame
    procedure ConfigEditor(ed: TSynEdit);
    procedure SetLanguage(idLang: string);
  public
    //genera constructor y destructor
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
//const
//  MAX_ARC_REC = 5;  //si se cambia, actualizar ActualMenusReciente()

{ TfraCfgSynEdit }
procedure TfraCfgSynEdit.Iniciar(section: string; cfgFile: TMiConfigXML);
begin
  //asigna referencia necesarias
  //crea las relaciones variable-control
  cfgFile.Asoc_TCol(section+ '/cTxtNor', @cTxtNor, cbutTextCol, clBlack);
  cfgFile.Asoc_TCol(section+ '/cFonEdi', @cFonEdi, cbutBackCol,  clWhite);
  cfgFile.Asoc_TCol(section+ '/cLinAct', @cLinAct, cbutLinAct, clYellow);
  cfgFile.Asoc_TCol(section+ '/cResPal', @cResPal, cbutResPal, clSkyBlue);

  cfgFile.Asoc_Bol(section+ '/VerBarDesV', @VerBarDesV, chkViewVScroll, true);
  cfgFile.Asoc_Bol(section+ '/VerBarDesH', @VerBarDesH, chkViewHScroll, false);
  cfgFile.Asoc_Bol(section+ '/Autoindent', @Autoindent, chkAutoindent, true);

  cfgFile.Asoc_Bol(section+ '/ResPalCur' , @ResPalCur , chkHighCurWord , true);
  cfgFile.Asoc_Bol(section+ '/MarLinAct' , @MarLinAct , chkHighCurLin , false);

  cfgFile.Asoc_Bol(section+ '/VerPanVer', @VerPanVer, chkVerPanVer, true);
  cfgFile.Asoc_Bol(section+ '/VerNumLin', @VerNumLin, chkVerNumLin, false);
  cfgFile.Asoc_Bol(section+ '/VerMarPle', @VerMarPle, chkVerMarPle, true);
  cfgFile.Asoc_TCol(section+ '/cFonPan'  , @cFonPan  , cbutFonPan  , clWhite);
  cfgFile.Asoc_TCol(section+ '/cTxtPan'  , @cTxtPan  , cbutTxtPan  , clBlack);

  cfgFile.Asoc_Int(section+ '/TamLet', @TamLet, spFontSize, 10);

  cmbFontName.Items.Clear;
  cmbFontName.Items.Add('Courier New');
  cmbFontName.Items.Add('Fixedsys');
  cmbFontName.Items.Add('Lucida Console');
  cmbFontName.Items.Add('Consolas');
  cmbFontName.Items.Add('Cambria');
  cfgFile.Asoc_Str(section+ '/TipLet', @TipLet, cmbFontName, 'Courier New');

  cfgFile.Asoc_StrList(section+ '/recient', @ArcRecientes);
end;

procedure TfraCfgSynEdit.chkVerPanVerChange(Sender: TObject);
begin
  chkVerNumLin.Enabled:=chkVerPanVer.Checked;
  chkVerMarPle.Enabled:=chkVerPanVer.Checked;
  cbutFonPan.Enabled:=chkVerPanVer.Checked;
  cbutTxtPan.Enabled:=chkVerPanVer.Checked;
  label2.Enabled:=chkVerPanVer.Checked;
  label3.Enabled:=chkVerPanVer.Checked;
end;
procedure TfraCfgSynEdit.chkHighCurLinChange(Sender: TObject);
begin
  label1.Enabled:=chkHighCurLin.Checked;
  cbutLinAct.Enabled:=chkHighCurLin.Checked;
end;

procedure TfraCfgSynEdit.chkHighCurWordChange(Sender: TObject);
begin
  label10.Enabled:=chkHighCurWord.Checked;
  cbutResPal.Enabled:=chkHighCurWord.Checked;
end;

procedure TfraCfgSynEdit.PropToWindow;
begin
   inherited;
   chkHighCurLinChange(self);  //para actualizar
   chkVerPanVerChange(self);  //para actualizar
end;
constructor TfraCfgSynEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ArcRecientes := TStringList.Create;  //crea lista
end;
destructor TfraCfgSynEdit.Destroy;
begin
  FreeAndNil(ArcRecientes);
  inherited Destroy;
end;
procedure TfraCfgSynEdit.ConfigEditor(ed: TSynEdit);
{Configura el editor con las propiedades almacenadas}
var
  marc: TSynEditMarkup;
begin
   if ed = nil then exit;  //protección
   //tipo de texto
   if TipLet <> '' then ed.Font.Name:=TipLet;
   if (TamLet > 6) and (TamLet < 32) then ed.Font.Size:=Round(TamLet);

   ed.Font.Color:=cTxtNor;      //color de texto normal
   ed.Color:=cFonEdi;           //color de fondo
   if MarLinAct then          //resaltado de línea actual
     ed.LineHighlightColor.Background:=cLinAct
   else
     ed.LineHighlightColor.Background:=clNone;
   //configura panel vertical
   ed.Gutter.Visible:=VerPanVer;  //muestra panel vertical
   ed.Gutter.Parts[1].Visible:=VerNumLin;  //Número de línea
   if ed.Gutter.Parts.Count>4 then
     ed.Gutter.Parts[4].Visible:=VerMarPle;  //marcas de plegado
   ed.Gutter.Color:=cFonPan;   //color de fondo del panel
   ed.Gutter.Parts[1].MarkupInfo.Background:=cFonPan; //fondo del núemro de línea
   ed.Gutter.Parts[1].MarkupInfo.Foreground:=cTxtPan; //texto del núemro de línea

   if VerBarDesV and VerBarDesH then  //barras de desplazamiento
     ed.ScrollBars:= ssBoth
   else if VerBarDesV and not VerBarDesH then
     ed.ScrollBars:= ssVertical
   else if not VerBarDesV and VerBarDesH then
     ed.ScrollBars:= ssHorizontal
   else
     ed.ScrollBars := ssNone;
   ////////Configura el resaltado de la palabra actual //////////
   marc := ed.MarkupByClass[TSynEditMarkupHighlightAllCaret];
   if marc<>nil then begin  //hay marcador
      marc.Enabled:=ResPalCur;  //configura
      marc.MarkupInfo.Background := cResPal;
   end;
   ///////fija color de delimitadores () {} [] ///////////
   ed.BracketMatchColor.Foreground := clRed;
   //Opciones
   if Autoindent then begin
     ed.Options := ed.Options + [eoAutoIndent];
   end else begin
     ed.Options := ed.Options - [eoAutoIndent];
   end;
end;
procedure TfraCfgSynEdit.SetLanguage(idLang: string);
//Rutina de traducción
begin
   curLang := idLang;
   {$I ..\language\tra_CfgSynEdit.pas}
end;

end.

