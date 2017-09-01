unit FrameRomExplorer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, StdCtrls, LCLProc,
  LCLIntf, LCLType, Pic16Utils;
type

  { TfraRomExplorer }

  TfraRomExplorer = class(TFrame)
    Label1: TLabel;
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
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
{ TfraRamExplorer }
procedure TfraRomExplorer.TextCent(x, y: integer; txt: string);
{Excribe texto centrado}
var
  ancTxt, altTxt: Integer;
begin
  altTxt := Canvas.TextHeight(txt);
  ancTxt := Canvas.TextWidth(txt);
  Canvas.Brush.Color := clWhite;
  Canvas.TextOut(x - ancTxt div 2, y - altTxt div 2, txt);
end;
procedure TfraRomExplorer.DibBar(const x1, x2: integer; y, alto: integer;
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
procedure TfraRomExplorer.DibBloque(const x1, x2: integer; y, alto: integer;
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
procedure TfraRomExplorer.DibPaginaROM(x, y, ancho, alto: integer; pag: TFlashPage;
                                      dirIni: integer);
begin
  //Dibuja fondo
  Canvas.Brush.Color := clWhite;
//  Canvas.Rectangle(x, y, x + ancho, y + alto);
  DibBloque(x, x+ancho, y, alto, 0, 2047, false);  //dibuja

//  for i := 0 to $7F do begin
//    if i = 0 then begin
//      //Define el bloque inicial
//      tipBloque := pag.ramPtr^[i + pag.AddrStart].state;
//      //tipBloque := pag.mem[i]^.state;
//      pos1 := i;
//      pos2 := i;
//    end else begin
//      if pag.ramPtr^[i + pag.AddrStart].state = tipBloque then begin
//      //if pag.mem[i]^.state = tipBloque then begin
//        //Es del bloque anterior
//        pos2 := i;   //actualiza límite
//      end else begin
//        //Es otro tipo de bloque.
//        //Cierra el anterior
//        numBytes := pos2 - pos1 + 1;
//        altBloq := round((numBytes/$80) * alto)+1;  //Suma 1, para corregir el hecho de subir 1 en "y"
//        DibBloque(x, x+ancho, y, altBloq, pos1, -1, tipBloque);  //dibuja
//        y := y + altBloq-1;  //nueva posición de inicio. Sube 1 para ni tener dos líneas juntas
//        //Define nuevo blqoue
//        //tipBloque := pag.mem[i]^.state;
//        tipBloque := pag.ramPtr^[i + pag.AddrStart].state;;
//        pos1 := i;
//        pos2 := i;
//      end;
//    end;
//  end;
//  //Cierra el último bloque
//  numBytes := pos2 - pos1 + 1;
//  altBloq := round((numBytes/$80) * alto);
//  DibBloque(x, x + ancho, y, altBloq, pos1, -1, tipBloque);  //dibuja
end;
procedure TfraRomExplorer.Frame1Paint(Sender: TObject);
var
  bordlat, ancPag, x0, i, separ, altur, bordSup, y0: Integer;
begin
  if pic = nil then exit;
  ////////////////////////
  //Espaciado entre bancos
  if width < 200 then begin
    //Ancho reducido
    separ := width div 24;  //espacio lateral
    bordlat := width div 24;
  end else begin
    //Ancho grande
    if (pic.NumPages=1) then begin
      //Es una sola página. Puede quedar mal proporcionada
      separ := width div 4;  //espacio lateral
      bordlat := width div 4;
    end else begin
      separ := width div 18;  //espacio lateral
      bordlat := width div 18;
    end;
  end;
  bordSup := (height-label1.Height) div 15;  //espacio superior
  ancPag := (width - bordlat * 2 - separ * (pic.NumPages-1)) div pic.NumPages;
  altur := (height-label1.Height) - 2* bordSup;
//debugln('width: %d bordLat: %d', [width, bordlat]);
  x0 := bordlat;
  y0 := bordsup + label1.Height;
  for i:=0 to pic.NumPages-1 do begin
    DibPaginaROM(x0, y0, ancPag, altur, pic.pages[i], i * $80);
    x0 := x0 + ancPag + separ;
  end;

end;

constructor TfraRomExplorer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.OnPaint := @Frame1Paint;
end;

destructor TfraRomExplorer.Destroy;
begin

  inherited Destroy;
end;

end.

