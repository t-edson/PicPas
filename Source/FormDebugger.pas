unit FormDebugger;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Grids, ActnList, Menus, Parser, FrameRamExplorer,
  FrameRomExplorer, FramePicRegisters, FrameRegWatcher, Pic16Utils;

type

  { TfrmDebugger }

  TfrmDebugger = class(TForm)
    acGenReset: TAction;
    acGenNext: TAction;
    acGenNextIn: TAction;
    acGenSetPC: TAction;
    ActionList1: TActionList;
    Image1: TImage;
    ImageList32: TImageList;
    ImageList16: TImageList;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    mnSetPCHere: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    panRAM: TPanel;
    panROM: TPanel;
    panStatis: TPanel;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure acGenNextExecute(Sender: TObject);
    procedure acGenNextInExecute(Sender: TObject);
    procedure acGenResetExecute(Sender: TObject);
    procedure acGenSetPCExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fraRamExp: TfraRamExplorer;
    fraRomExp: TfraRomExplorer;
    fraPicReg: TfraPicRegisters;
    fraRegWat: TfraRegWatcher;
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
  public
    pic: TPIC16;
    procedure Exec(cxp: TCompiler);
  end;

var
  frmDebugger: TfrmDebugger;

implementation

{$R *.lfm}

{ TfrmDebugger }
procedure TfrmDebugger.FormCreate(Sender: TObject);
begin
  fraRamExp:= TfraRamExplorer.Create(self);
  fraRamExp.Parent := panRAM;
  fraRamExp.Align := alClient;

  fraRomExp:= TfraRomExplorer.Create(self);
  fraRomExp.Parent := panROM;
  fraRomExp.Align := alClient;

  fraPicReg:= TfraPicRegisters.Create(self);
  fraPicReg.Parent := panStatis;
  fraPicReg.Align := alClient;

  fraRegWat := TfraRegWatcher.Create(self);
  fraRegWat.Parent := Panel2;
  fraRegWat.Align := alClient;

  //Configura Toolbar
//  ToolBar1.ButtonHeight:=38;
//  ToolBar1.ButtonWidth:=38;
//  ToolBar1.Height:=42;
//  ToolBar1.Images:=ImgActions32;
end;

procedure TfrmDebugger.StringGrid1DrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  txt: String;           // texto de la celda
  cv: TCanvas;           //referencia al lienzo
  valp: word;
begin
  cv := StringGrid1.Canvas;  //referencia al Lienzo

  if gdFixed in aState then begin
    //Es una celda fija
    cv.Brush.Color := clMenu;      // le ponemos azul de fondo
    cv.Font.Color := clBlack;      // fuente blanca
    cv.Font.Style := [];     // y negrita
  end else begin
    //Es una celda común
    if aRow = StringGrid1.Row then begin
      //Fila seleccionada
      cv.Brush.Color := clGray;  // fondo marrón
      cv.Font.Color := clBlack;    // letra blanca
      cv.Font.Style := [fsBold];   // negrita
    end else begin
      //Celda sin selección
      if pic.flash[aRow].used then begin
        cv.Brush.Color := clWhite;  //fondo blanco
      end else begin
        cv.Brush.Color := $E0E0E0;
      end;
      cv.Font.Color := clGreen;   //letra verde
      cv.Font.Style := [fsBold];  //negrita
    end;
  end;
  //Dibuja contenido de celda
  cv.FillRect(aRect);   //fondo
  if ACol = 0 then begin
    txt := '$'+IntToHex(aRow,3);
    cv.TextOut(aRect.Left + 2, aRect.Top + 2, txt);
  end else if ACol = 1then begin
    //Columna de marca
    if aRow = pic.PCL then begin  //marca
       ImageList16.Draw(cv, aRect.Left + 4, aRect.Top+2, 3);
    end;
  end else if ACol = 2 then begin
    //Celda normal
    valp := pic.flash[aRow].value;
    pic.Decode(valp);   //decodifica instrucción
    txt := pic.Disassembler(true);
    //Escribe texto con alineación
    cv.TextOut(aRect.Left + 2, aRect.Top + 2, txt);
  end;
end;
procedure TfrmDebugger.acGenResetExecute(Sender: TObject);
var
  pc: Byte;
begin
  pic.Reset;
  pc := pic.PCL;
  StringGrid1.Row := pc;
  StringGrid1.Invalidate;  //Para que actualice marca de PCL
  fraPicReg.Refrescar;
  if fraRamExp.Visible then fraRamExp.panGraph.Invalidate;
  fraRegWat.Refrescar;
end;
procedure TfrmDebugger.acGenSetPCExecute(Sender: TObject);
//Fija el puntero del programa en la instrucción seleccionad
begin
  if StringGrid1.Row=-1 then exit;
  pic.PCL := StringGrid1.Row and $FF;
  pic.PCH := StringGrid1.Row >> 8;
end;
procedure TfrmDebugger.acGenNextExecute(Sender: TObject);
var
  pc: Byte;
begin
  pic.Exec();
  pc := pic.PCH*256 + pic.PCL;
  StringGrid1.Row := pc;
  StringGrid1.Invalidate;  //Para que actualice marca de PCL
  fraPicReg.Refrescar;
  if fraRamExp.Visible then fraRamExp.panGraph.Invalidate;
  fraRegWat.Refrescar;
end;
procedure TfrmDebugger.acGenNextInExecute(Sender: TObject);
begin
  acGenNextExecute(self);
end;

procedure TfrmDebugger.Exec(cxp: TCompiler);
begin
  pic := cxp.pic;
  StringGrid1.DefaultDrawing:=false;
  StringGrid1.OnDrawCell := @StringGrid1DrawCell;

  //Muestra Frames
  fraRamExp.pic := pic;
  fraRamExp.panGraph.Invalidate;
  fraRomExp.pic := pic;
  fraRomExp.Invalidate;
  fraPicReg.pic := pic;
  fraPicReg.Invalidate;
  fraRegWat.pic := pic;
  fraRegWat.Refrescar;

  self.Show;
end;

end.

