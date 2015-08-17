{Unidad con frame para almacenar y configurar las propiedades de un editor
 SynEdit. Las propiedades que se manejan son con respecto al coloreado.
 El frame definido, está pensado para usarse en una ventana de configuración.
 También incluye una lista para almacenamiento de los archivos recientes
                               Por Tito Hinostroza  23/11/2013
}
unit FrameCfgEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, Dialogs,
  Spin, SynEdit, Graphics, SynEditMarkupHighAll, SynEditMarkup
  ,ConfigFrame;   //para interceptar TFrame;
type

  { TfcEdit }
  TfcEdit = class(TFrame)
    cbutFonPan: TColorButton;
    cbutResPal: TColorButton;
    cbutTxtPan: TColorButton;
    chkResPalCur: TCheckBox;
    chkVerBarDesH: TCheckBox;
    chkVerPanVer: TCheckBox;
    chkMarLinAct: TCheckBox;
    cbutLinAct: TColorButton;
    chkVerBarDesV: TCheckBox;
    chkVerNumLin: TCheckBox;
    chkVerMarPle: TCheckBox;
    cmbTipoLetra: TComboBox;
    cbutFondo: TColorButton;
    cbutTexto: TColorButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    spTam: TSpinEdit;
    procedure chkMarLinActChange(Sender: TObject);
    procedure chkResPalCurChange(Sender: TObject);
    procedure chkVerPanVerChange(Sender: TObject);
    //genera constructor y destructor
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  private
    ed: TSynEdit;
    procedure ConfigEditor;
  public
    //configuración del editor
    TipLet     : string;     //tipo de letra
    TamLet     : integer;     //tamaño de letra
    MarLinAct : boolean;    //marcar línea actual
    VerBarDesV  : boolean;    //ver barras de desplazamiento
    VerBarDesH  : boolean;    //ver barras de desplazamiento
    ResPalCur  : boolean;    //resaltar palabra bajo el cursor
    cTxtNor     : TColor;    //color de texto normal
    cFonEdi     : TColor;    //Color de fondo del control de edición
    cFonSel     : TColor;    //color del fondo de la selección
    cTxtSel     : TColor;    //color del texto de la selección
    cLinAct     : TColor;    //color de la línea actual
    cResPal     : TColor;    //color de la palabra actual
    //panel vertical
    VerPanVer  : boolean;    //ver pánel vertical
    VerNumLin  : boolean;    //ver número de línea
    VerMarPle  : boolean;    //ver marcas de plegado
    cFonPan     : TColor;    //color de fondo del panel vertical
    cTxtPan     : TColor;    //color de texto del panel vertical
    ArcRecientes: TStringList;  //Lista de archivos recientes

    procedure PropToWindow; override;
    procedure Iniciar(secINI0: string; ed0: TSynEdit; colFonDef: TColor=clWhite); //Inicia el frame
    procedure SetLanguage(lang: string);
  end;

implementation
{$R *.lfm}
//const
//  MAX_ARC_REC = 5;  //si se cambia, actualizar ActualMenusReciente()

{ TfcEdit }
procedure TfcEdit.Iniciar(secINI0: string; ed0: TSynEdit;
                  colFonDef: TColor = clWhite);
begin
  secINI := secINI0;  //sección INI
  //asigna referencia necesarias
  ed := ed0;
  OnUpdateChanges := @ConfigEditor;  //manejador de cambios
  //crea las relaciones variable-control
  Asoc_Col_TColBut(@cTxtNor, cbutTexto, 'cTxtNor',clBlack);
  Asoc_Col_TColBut(@cFonEdi, cbutFondo, 'cFonEdi',colFonDef);
  Asoc_Col_TColBut(@cLinAct, cbutLinAct, 'cLinAct',clYellow);
  Asoc_Col_TColBut(@cResPal, cbutResPal, 'cResPal',clSkyBlue);

  Asoc_Bol_TChkBox(@VerBarDesV,chkVerBarDesV,'VerBarDesV',true);
  Asoc_Bol_TChkBox(@VerBarDesH,chkVerBarDesH,'VerBarDesH',false);
  Asoc_Bol_TChkBox(@ResPalCur,chkResPalCur,'ResPalCur',true);
  Asoc_Bol_TChkBox(@MarLinAct,chkMarLinAct,'MarLinAct',false);

  Asoc_Bol_TChkBox(@VerPanVer, chkVerPanVer, 'VerPanVer',true);
  Asoc_Bol_TChkBox(@VerNumLin, chkVerNumLin, 'VerNumLin',false);
  Asoc_Bol_TChkBox(@VerMarPle, chkVerMarPle, 'VerMarPle',true);
  Asoc_Col_TColBut(@cFonPan, cbutFonPan, 'cFonPan',colFonDef);
  Asoc_Col_TColBut(@cTxtPan, cbutTxtPan, 'cTxtPan',clBlack);

  Asoc_Int_TSpinEdit(@TamLet, spTam, 'TamLet', 10, 5, 20);

  cmbTipoLetra.Items.Clear;
  cmbTipoLetra.Items.Add('Courier New');
  cmbTipoLetra.Items.Add('Fixedsys');
  cmbTipoLetra.Items.Add('Lucida Console');
  cmbTipoLetra.Items.Add('Consolas');
  cmbTipoLetra.Items.Add('Cambria');
  Asoc_Str_TCmbBox(@TipLet, cmbTipoLetra, 'TipLet', 'Courier New');

  Asoc_StrList(@ArcRecientes, 'recient');
end;

procedure TfcEdit.chkVerPanVerChange(Sender: TObject);
begin
  chkVerNumLin.Enabled:=chkVerPanVer.Checked;
  chkVerMarPle.Enabled:=chkVerPanVer.Checked;
  cbutFonPan.Enabled:=chkVerPanVer.Checked;
  cbutTxtPan.Enabled:=chkVerPanVer.Checked;
  label2.Enabled:=chkVerPanVer.Checked;
  label3.Enabled:=chkVerPanVer.Checked;
end;
procedure TfcEdit.chkMarLinActChange(Sender: TObject);
begin
  label1.Enabled:=chkMarLinAct.Checked;
  cbutLinAct.Enabled:=chkMarLinAct.Checked;
end;

procedure TfcEdit.chkResPalCurChange(Sender: TObject);
begin
  label10.Enabled:=chkResPalCur.Checked;
  cbutResPal.Enabled:=chkResPalCur.Checked;
end;

procedure TfcEdit.PropToWindow;
begin
   inherited;
   chkMarLinActChange(self);  //para actualizar
   chkVerPanVerChange(self);  //para actualizar
end;
constructor TfcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ArcRecientes := TStringList.Create;  //crea lista
end;
destructor TfcEdit.Destroy;
begin
  FreeAndNil(ArcRecientes);
  inherited Destroy;
end;
procedure TfcEdit.ConfigEditor;
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
end;
procedure TfcEdit.SetLanguage(lang: string);
//Rutina de traducción
begin
  case lowerCase(lang) of
  'es': begin
      Label6.Caption:='&Letra:';
      Label7.Caption:='&Tamaño:';
      Label8.Caption:='Color de fondo:';
      Label9.Caption:='Color texto:';
      chkVerBarDesV.Caption:='Barra de desplaz &Vert.';
      chkVerBarDesH.Caption:='Barra de desplaz &Horiz.';
      chkResPalCur.Caption:='Resaltar palabra bajo cursor';
      chkMarLinAct.Caption:='Marcar línea actual';
      chkVerPanVer.Caption:='Panel Vertical';
      chkVerNumLin.Caption:='Ver Núm.de línea';
      label2.Caption:='Color Fondo:';
      chkVerMarPle.Caption:='Ver Marc.de plegado';
      label3.Caption:='Color de texto:';
    end;
  'en': begin
      Label6.Caption:='&Font:';
      Label7.Caption:='&Size:';
      Label8.Caption:='Back color:';
      Label9.Caption:='Font Color:';
      chkVerBarDesV.Caption:='&Vertical Scrollbar';
      chkVerBarDesH.Caption:='&Horizontal Scrollbar';
      chkResPalCur.Caption:='Highlight current word';
      chkMarLinAct.Caption:='Hightlight current line';
      chkVerPanVer.Caption:='Gutter';
      chkVerNumLin.Caption:='Show line number';
      label2.Caption:='Back color:';
      chkVerMarPle.Caption:='Show folding marks';
      label3.Caption:='Text color:';
    end;
  end;
end;

end.

