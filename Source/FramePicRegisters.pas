unit FramePicRegisters;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, StdCtrls, LCLProc,
  LCLIntf, LCLType, Grids, ExtCtrls, Pic16Utils;
type

  { TfraPicRegisters }

  TfraPicRegisters = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
  private
    procedure DibPaginaROM(x, y, ancho, alto: integer; pag: TFlashPage;
      dirIni: integer);
    procedure DibBar(const x1, x2: integer; y, alto: integer; dirIni,
      dirFin: integer; lbl: string);
    procedure DibBloque(const x1, x2: integer; y, alto: integer; dirIni,
      dirFin: integer; used: boolean);
    procedure Frame1Paint(Sender: TObject);
    procedure TextCEnt(x, y: integer; txt: string);
  public
    pic: TPIC16;
    procedure Refrescar;
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
{ TfraPicRegisters }
procedure TfraPicRegisters.TextCEnt(x, y: integer; txt: string);
{Excribe texto centrado}
var
  ancTxt, altTxt: Integer;
begin
  altTxt := Canvas.TextHeight(txt);
  ancTxt := Canvas.TextWidth(txt);
  Canvas.Brush.Color := clWhite;
  Canvas.TextOut(x - ancTxt div 2, y - altTxt div 2, txt);
end;
procedure TfraPicRegisters.Refrescar;
{Refresca valores de los registros}
var
  w0,w1,w2,w3,w4,w5,w6,w7: Byte;
begin
  w0 := (pic.W and %00000001);
  w1 := (pic.W and %00000010)>>1;
  w2 := (pic.W and %00000100)>>2;
  w3 := (pic.W and %00001000)>>3;
  w4 := (pic.W and %00010000)>>4;
  w5 := (pic.W and %00100000)>>5;
  w6 := (pic.W and %01000000)>>6;
  w7 := (pic.W and %10000000)>>7;
  StringGrid1.Cells[0,1] := IntToStr(pic.W);
  StringGrid1.Cells[1,1] := IntToHex(pic.W, 2);
  StringGrid1.Cells[2,1] := IntToStr(w7);
  StringGrid1.Cells[3,1] := IntToStr(w6);
  StringGrid1.Cells[4,1] := IntToStr(w5);
  StringGrid1.Cells[5,1] := IntToStr(w4);
  StringGrid1.Cells[6,1] := IntToStr(w3);
  StringGrid1.Cells[7,1] := IntToStr(w2);
  StringGrid1.Cells[8,1] := IntToStr(w1);
  StringGrid1.Cells[9,1] := IntToStr(w0);

  w0 := (pic.STATUS and %00000001);
  w1 := (pic.STATUS and %00000010)>>1;
  w2 := (pic.STATUS and %00000100)>>2;
  w3 := (pic.STATUS and %00001000)>>3;
  w4 := (pic.STATUS and %00010000)>>4;
  w5 := (pic.STATUS and %00100000)>>5;
  w6 := (pic.STATUS and %01000000)>>6;
  w7 := (pic.STATUS and %10000000)>>7;
  StringGrid2.Cells[0,1] := IntToStr(pic.STATUS);
  StringGrid2.Cells[1,1] := IntToHex(pic.STATUS, 2);
  StringGrid2.Cells[2,1] := IntToStr(w7);
  StringGrid2.Cells[3,1] := IntToStr(w6);
  StringGrid2.Cells[4,1] := IntToStr(w5);
  StringGrid2.Cells[5,1] := IntToStr(w4);
  StringGrid2.Cells[6,1] := IntToStr(w3);
  StringGrid2.Cells[7,1] := IntToStr(w2);
  StringGrid2.Cells[8,1] := IntToStr(w1);
  StringGrid2.Cells[9,1] := IntToStr(w0);

end;
procedure TfraPicRegisters.DibBar(const x1, x2: integer; y, alto: integer;
                                 dirIni, dirFin: integer; lbl: string);
//Dibuja una barra, en la posición: x1, x2, y, con altura "alto".
var
  altTxt, ancho, ancTxt, xt, yt: Integer;
  ARect: TRect;
  TextStyle: TTextStyle;
begin
  Canvas.Rectangle(x1, y, x2, y + alto);
  //////// Escribe etiqueta centrada /////////////
  ancho := x2 - x1;
  altTxt := Canvas.TextHeight(lbl);
  ancTxt := Canvas.TextWidth(lbl);
  ARect.Left := x1;
  ARect.Right := x2;
  ARect.Top := y;
  ARect.Bottom := y + alto;
  if ancTxt> ancho then
     xt := x1  //no centra
  else
     xt := x1 + ancho div 2 -  ancTxt div 2;  //centra
  yt := y + alto div 2 - altTxt div 2;
//  Canvas.Pen.Color := clred;
  TextStyle := Canvas.TextStyle;
  TextStyle.EndEllipsis := true;
  Canvas.Brush.Style := bsClear;  //texto sin fondo
  Canvas.TextRect(Arect, xt, yt, lbl, TextStyle);

end;
procedure TfraPicRegisters.DibBloque(const x1, x2: integer; y, alto: integer;
                                 dirIni, dirFin: integer;
                                 used: boolean);
{Dibuja un bloque, de memoria RAM, pone etqiueta descriptiva y pinta con color
indicativo.}
var
  lbl, etiqIni, etiqFin: String;
  ancMargenDir,
  altTxt: integer;  //Ancho de margen para las etiquetas de dirección.
begin
  case used of
  true: begin
    Canvas.Brush.Color := clGray;
    lbl := 'Used';
  end;
  false: begin
    Canvas.Brush.Color := clWhite;
    lbl := 'Free';
  end;
  end;
  //Convierte direcciones a texto y calcula ancho de margen de direcciones
  ancMargenDir := 0;  //Por defecto
  if (alto>20) and (dirIni<>-1) then begin
    etiqIni := '$'+IntToHex(dirIni, 3);
  end;
  if (alto>20) and (dirFin<>-1) then begin
    etiqFin := '$'+IntToHex(dirFin, 3);
    altTxt := Canvas.TextHeight(etiqFin);
  end;
  ancMargenDir := canvas.TextWidth(etiqIni);
  //Dibuja barra de fondo y direcciones
  if ancMargenDir>(x2-x1)*0.5 then begin
    //El ancho es muy pequeño para dibujar direcciones
    DibBar(x1, x2, y, alto, dirIni, dirFin, lbl);
  end else begin
    DibBar(x1+ancMargenDir, x2, y, alto, dirIni, dirFin, lbl);
    //Dibuja etiquetas de dirección
    Canvas.TextOut(x1, y+1, etiqIni);
    Canvas.TextOut(x1, y+alto-altTxt-1, etiqFin);
  end;
end;
procedure TfraPicRegisters.DibPaginaROM(x, y, ancho, alto: integer; pag: TFlashPage;
                                      dirIni: integer);
begin
  //Dibuja fondo
  Canvas.Brush.Color := clWhite;
//  Canvas.Rectangle(x, y, x + ancho, y + alto);
  DibBloque(x, x+ancho, y, alto, 0, 2047, false);  //dibuja

end;
procedure TfraPicRegisters.Frame1Paint(Sender: TObject);
//var
//  bordlat, separ: Integer;
begin
  if pic = nil then exit;
  ////////////////////////
  //Espaciado entre bancos
//  if width < 200 then begin
//    //Ancho reducido
//    separ := width div 24;  //espacio lateral
//    bordlat := width div 24;
//  end else begin
//    //Ancho grande
//    if (pic.NumPages=1) then begin
//      //Es una sola página. Puede quedar mal proporcionada
//      separ := width div 4;  //espacio lateral
//      bordlat := width div 4;
//    end else begin
//      separ := width div 18;  //espacio lateral
//      bordlat := width div 18;
//    end;
//  end;
//  bordSup := (height-label1.Height) div 15;  //espacio superior
//  ancPag := (width - bordlat * 2 - separ * (pic.NumPages-1)) div pic.NumPages;
//  altur := (height-label1.Height) - 2* bordSup;
//
//  x0 := bordlat;
//  y0 := bordsup + label1.Height;
//  for i:=0 to pic.NumPages-1 do begin
//    DibPaginaROM(x0, y0, ancPag, altur, pic.pages[i], i * $80);
//    x0 := x0 + ancPag + separ;
//  end;

end;

constructor TfraPicRegisters.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.OnPaint := @Frame1Paint;
end;
destructor TfraPicRegisters.Destroy;
begin

  inherited Destroy;
end;

end.

