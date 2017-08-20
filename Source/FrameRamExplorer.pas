unit FrameRamExplorer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, StdCtrls, LCLProc,
  Pic16Utils;

type

  { TfraRamExplorer }

  TfraRamExplorer = class(TFrame)
    Label1: TLabel;
  private
    x1, x2: integer;
    procedure DibBancoRAM(x, y, ancho, alto: integer; bnk: TRAMBank; dirIni: integer
      );
    procedure DibBar(y, alto: integer; dirIni, dirFin: integer);
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
  ancTxt, altTxt: Integer;
begin
  altTxt := Canvas.TextHeight(txt);
  ancTxt := Canvas.TextWidth(txt);
  Canvas.Brush.Color := clWhite;
  Canvas.TextOut(x - ancTxt div 2, y, txt);
end;
procedure TfraRamExplorer.DibBar(y, alto: integer; dirIni, dirFin: integer);
//Dibuja una barra, en la posición y, con altura "alto".
var
  etiq: String;
  altTxt: Integer;
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
end;
procedure TfraRamExplorer.DibBancoRAM(x, y, ancho, alto: integer; bnk: TRAMBank;
                                      dirIni: integer);
var
  cv: TCanvas;
  altoSFR, altoGPR: integer;
begin
  cv := self.Canvas;
  //Dibuja fondo
//  cv.Pen.Color := clBlack;
  cv.Brush.Color := clRed;
  cv.Rectangle(x, y, x + ancho, y + alto);
  altoSFR := round((bnk.GPRStart / $80) * alto);
  altoGPR := round((bnk.TotalGPR / $80) * alto);
  x1 := x;  x2 := x + ancho;

  //Dibuja SFR
  cv.Pen.Color := clBlack;
  cv.Brush.Color := $E0E0E0;
  DibBar(y, altoSFR, dirIni + 0, -1);

  //Dibuja GPR
  y := y + altoSFR-1;  //nueva posición de inicio
  //altoGPR := alto - altoSFR+1;
  cv.Pen.Color := clBlack;
  cv.Brush.Color := clLtGray;
  DibBar(y, altoGPR, dirIni+ bnk.GPRStart, dirIni + bnk.GPRStart+bnk.TotalGPR-1);

  //Dibuja restantes

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

