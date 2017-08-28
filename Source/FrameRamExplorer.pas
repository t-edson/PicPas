unit FrameRamExplorer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, StdCtrls, LCLProc,
  LCLIntf, LCLType, Pic16Utils;

type

  { TfraRamExplorer }

  TfraRamExplorer = class(TFrame)
    Label1: TLabel;
  private
//    x1, x2: integer;
    procedure DibBancoRAM(x, y, ancho, alto: integer; bnk: TRAMBank; dirIni: integer
      );
    procedure DibBar(const x1, x2: integer; y, alto: integer; dirIni,
      dirFin: integer; lbl: string);
    procedure DibBloque(const x1, x2: integer; y, alto: integer; dirIni,
      dirFin: integer; blkType: TPIC16CellState);
    procedure Frame1Paint(Sender: TObject);
    procedure TextCEnt(x, y: integer; txt: string);
    procedure TextCentX(x, y: integer; txt: string);
    { private declarations }
  public
    pic: TPIC16;
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}
{ TfraRamExplorer }
procedure TfraRamExplorer.TextCent(x, y: integer; txt: string);
{Excribe texto centrado}
var
  ancTxt, altTxt: Integer;
begin
  altTxt := Canvas.TextHeight(txt);
  ancTxt := Canvas.TextWidth(txt);
  Canvas.Brush.Color := clWhite;
  Canvas.TextOut(x - ancTxt div 2, y - altTxt div 2, txt);
end;
procedure TfraRamExplorer.TextCentX(x, y: integer; txt: string);
{Excribe texto centrado}
var
  ancTxt: Integer;
begin
//  altTxt := Canvas.TextHeight(txt);
  ancTxt := Canvas.TextWidth(txt);
  Canvas.Brush.Color := clWhite;
  Canvas.TextOut(x - ancTxt div 2, y, txt);
end;
procedure TfraRamExplorer.DibBar(const x1, x2: integer; y, alto: integer;
                                 dirIni, dirFin: integer; lbl: string);
//Dibuja una barra, en la posición: x1, x2, y, con altura "alto".
var
  etiq: String;
  altTxt, ancho, ancTxt, xt, yt: Integer;
  ARect: TRect;
  TextStyle: TTextStyle;
begin
  Canvas.Rectangle(x1, y, x2, y + alto);
  if (alto>20) and (dirIni<>-1) then begin
    etiq := '$'+IntToHex(dirIni, 2);
    Canvas.TextOut(x1+2, y+1, etiq);
  end;
  if (alto>20) and (dirFin<>-1) then begin
    etiq := '$'+IntToHex(dirFin, 2);
    altTxt := Canvas.TextHeight(etiq);
    Canvas.TextOut(x1+2, y+alto-altTxt-1, etiq);
  end;
  //////// Escribe etiqueta centrada /////////////
  ancho := x2 - x1;
  altTxt := Canvas.TextHeight(lbl);
  ancTxt := Canvas.TextWidth(lbl);
  ARect.Left := x1;
  ARect.Right := x2;
  ARect.Top := y;
  ARect.Bottom := y + alto;
  xt := x1 + ancho div 2 -  ancTxt div 2;
  yt := y + alto div 2 - altTxt div 2;
//  Canvas.Pen.Color := clred;
  TextStyle := Canvas.TextStyle;
  TextStyle.EndEllipsis := true;
  Canvas.TextRect(Arect, xt, yt, lbl, TextStyle);

end;
procedure TfraRamExplorer.DibBloque(const x1, x2: integer; y, alto: integer;
                                 dirIni, dirFin: integer;
                                 blkType: TPIC16CellState);
{Dibuja un bloque, de memoria RAM, pone etqiueta descriptiva y pinta con color
indicativo.}
var
  lbl: String;
  TextStyle: TTextStyle;
begin
  case blkType of
  cs_impleSFR: begin
    Canvas.Brush.Color := clBlue;
    lbl := 'SFR';
  end;
  cs_impleGPR: begin
    Canvas.Brush.Color := clWhite;
    lbl := 'GPR';
  end;
  cs_unimplem: begin
    Canvas.Brush.Color := clGray;
    lbl := 'Uninplemented';
  end;
  cs_mapToBnk: begin
    Canvas.Brush.Color := clGreen;
    lbl := 'Mapped to';
  end;
  end;
  DibBar(x1, x2, y, alto, dirIni, dirFin, lbl);

end;
procedure TfraRamExplorer.DibBancoRAM(x, y, ancho, alto: integer; bnk: TRAMBank;
                                      dirIni: integer);
var
  cv: TCanvas;
  altoSFR, altoGPR, pos1, i, pos2: integer;
  tipBloque: TPIC16CellState;
  altBloq, numBytes: integer;
begin
  cv := self.Canvas;
  //Dibuja fondo
  cv.Brush.Color := clRed;
  cv.Rectangle(x, y, x + ancho, y + alto);
  altoSFR := round((bnk.GPRStart / $80) * alto);
  altoGPR := round((bnk.TotalGPR / $80) * alto);

//  //Dibuja SFR
//  cv.Pen.Color := clBlack;
//  cv.Brush.Color := $E0E0E0;
//  DibBar(y, altoSFR, dirIni + 0, -1);
//
//  //Dibuja GPR
//  y := y + altoSFR-1;  //nueva posición de inicio
//  cv.Pen.Color := clBlack;
//  cv.Brush.Color := clLtGray;
//  DibBar(y, altoGPR, dirIni+ bnk.GPRStart, dirIni + bnk.GPRStart+bnk.TotalGPR-1);
  for i := 0 to $7F do begin
    if i = 0 then begin
      //Define el bloque inicial
      tipBloque := bnk.mem[i]^.state;
      pos1 := i;
      pos2 := i;
    end else begin
      //if bnk.ramPtr^[i + bnk.AddrStart].state = tipBloque then begin
      if bnk.mem[i]^.state = tipBloque then begin
        //Es del bloque anterior
        pos2 := i;   //actualiza límite
      end else begin
        //Es otro tipo de bloque.
        //Cierra el anterior
        numBytes := pos2 - pos1 + 1;
        altBloq := round((numBytes/$80) * alto);
        DibBloque(x, x+ancho, y, altBloq, pos1, -1, tipBloque);  //dibuja
        y := y + altBloq-1;  //nueva posición de inicio
        //Define nuevo blqoue
        tipBloque := bnk.mem[i]^.state;
        pos1 := i;
        pos2 := i;
      end;
    end;
  end;
  //Cierra el último bloque
  numBytes := pos2 - pos1 + 1;
  altBloq := round((numBytes/$80) * alto);
  DibBloque(x, x + ancho, y, altBloq, pos1, -1, tipBloque);  //dibuja

end;
procedure TfraRamExplorer.Frame1Paint(Sender: TObject);
var
  cv: TCanvas;
  bordlat, ancPag, x0, i, separ, altur, bordSup, y0: Integer;
begin
  cv := self.Canvas;
  if pic = nil then exit;
  ////////////////////////
  bordlat := width div 12;  //espacio lateral
  bordSup := (height-label1.Height) div 15;  //espacio superior
  separ := width div 12;  //espacio lateral
  ancPag := (width - bordlat * 2 - separ * (pic.NumBanks-1)) div pic.NumBanks;
  altur := (height-label1.Height) - 2* bordSup;
//debugln('width: %d bordLat: %d', [width, bordlat]);
  x0 := bordlat;
  y0 := bordsup + label1.Height;
  for i:=0 to pic.NumBanks-1 do begin
    DibBancoRAM(x0, y0, ancPag, altur, pic.banks[i], i * $80);
    //cv.Rectangle(x0, y0, x0 + ancPag, y0 + altur);
    x0 := x0 + ancPag + separ;
  end;

end;

constructor TfraRamExplorer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.OnPaint := @Frame1Paint;
end;

destructor TfraRamExplorer.Destroy;
begin

  inherited Destroy;
end;

end.

