unit FormDebugger;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Grids, ActnList, Parser, FrameRamExplorer,
  FrameRomExplorer, FramePicRegisters, Pic16Utils;

type

  { TfrmDebugger }

  TfrmDebugger = class(TForm)
    acGenReset: TAction;
    acGenNext: TAction;
    acGenNextIn: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    ImageList32: TImageList;
    ImageList16: TImageList;
    Label1: TLabel;
    Panel1: TPanel;
    panRAM: TPanel;
    panROM: TPanel;
    panStatis: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure acGenNextExecute(Sender: TObject);
    procedure acGenNextInExecute(Sender: TObject);
    procedure acGenResetExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fraRamExp: TfraRamExplorer;
    fraRomExp: TfraRomExplorer;
    fraPicReg: TfraPicRegisters;
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
  ancTxt: Integer;       // ancho del texto
  Alineacion: TAlignment;
  cv: TCanvas;           //referencia al lienzo
begin
  cv := StringGrid1.Canvas;  //referencia al Lienzo

  if gdFixed in aState then begin
    //Es una celda fija
    Alineacion := taCenter;
    cv.Brush.Color := clMenu;      // le ponemos azul de fondo
    cv.Font.Color := clBlack;      // fuente blanca
    cv.Font.Style := [];     // y negrita
  end else begin
    //Es una celda común
    if ACol = 0 then  //columna numérica
      Alineacion := taRightJustify  //a la derecha
    else
      Alineacion := taLeftJustify;  //cualquier otra

//    if gdFocused in aState then begin
    if aRow = StringGrid1.Row then begin
      //Fila seleccionada
      cv.Brush.Color := clGray;  // fondo marrón
      cv.Font.Color := clBlack;    // letra blanca
      cv.Font.Style := [fsBold];   // negrita
    end else begin
      //Celda sin selección
      cv.Brush.Color := clWhite;  //fondo blanco
      if ACol = 2 then begin        //Opcode
        cv.Font.Color := clGreen;   //letra verde
        cv.Font.Style := [fsBold];  //negrita
      end else begin
        cv.Font.Color := clBlack;
        cv.Font.Style := [];
      end;
    end;
  end;
  //Dibuja contenido de celda
  if ACol = 1 then begin
    //Columna de marca
    cv.FillRect(aRect);   //fondo
    cv.Font.Color := clGreen;   //letra verde
    if aRow = pic.PCL then begin  //marca
      cv.TextOut(aRect.Left + 2, aRect.Top + 2, '*');
    end;
  end else begin
    //Celda normal
    txt := StringGrid1.Cells[ACol,ARow];
    cv.FillRect(aRect);   //fondo
    ancTxt := cv.TextWidth(txt);
    //escribe texto con alineación
    case Alineacion of
      taLeftJustify:
        cv.TextOut(aRect.Left + 2, aRect.Top + 2, txt);
      taCenter:
        cv.TextOut(aRect.Left + ((aRect.Right - aRect.Left) - ancTxt) div 2,
                   aRect.Top + 2, txt );
      taRightJustify:
        cv.TextOut(aRect.Right - ancTxt - 2, aRect.Top + 2, txt);
    end;
  end;
end;
procedure TfrmDebugger.Button1Click(Sender: TObject);
var
  i: Integer;
  pag: TFlashPage;
  val: Word;
  lin: String;
  p: SizeInt;
begin
  StringGrid1.BeginUpdate;
  pag := pic.page0;
  for i:=0 to 2047 do begin
    StringGrid1.Cells[0, i] := '0x' + IntToHex(i,3);
    if i<pag.maxUsed then begin
      val := pag.mem[i].value;
      pic.Decode(val);   //decodifica instrucción
      lin := pic.Disassembler(true);
      p := pos(' ', lin);
      if p=0 then begin  //No hay operando
        StringGrid1.Cells[2, i] := lin;
        StringGrid1.Cells[3, i] := '';
      end else begin
        StringGrid1.Cells[2, i] := copy(lin, 1, p-1);
        StringGrid1.Cells[3, i] := copy(lin, p+1, 100);
      end;
    end else begin
      StringGrid1.Cells[1, i] := '';
    end;
  end;
  StringGrid1.EndUpdate();
end;

procedure TfrmDebugger.acGenResetExecute(Sender: TObject);
var
  pc: Byte;
begin
  pic.Reset;
  pc := pic.PCL;
  StringGrid1.Row := pc;
  StringGrid1.Invalidate;  //Para que actualice marca de PCL
  fraPicReg.Refresh;
end;
procedure TfrmDebugger.acGenNextExecute(Sender: TObject);
var
  pc: Byte;
begin
  pic.Exec();
  pc := pic.PCL;
  StringGrid1.Row := pc;
  StringGrid1.Invalidate;  //Para que actualice marca de PCL
  fraPicReg.Refresh;
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
  fraRamExp.Invalidate;
  fraRomExp.pic := pic;
  fraRomExp.Invalidate;
  fraPicReg.pic := pic;
  fraPicReg.Invalidate;

  self.Show;
end;

end.

