unit FrameRomExplorer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, StdCtrls, LCLProc,
  LCLIntf, LCLType, ExtCtrls, PicCore, CompBase;
type
  //Define a un bloque de RAM, que servirá para dibujo
  {Los bloques de RAM se usan para separar la memoria en bloques de acuerdo a
  diversos criterios}
  TRomBlock = record
    add1, add2 : word;     //Direcciones de memoria
    implemented: boolean;  //Indica si está implementado
    used       : boolean;  //Indica si el bloque está usado
  end;

  { TfraRomExplorer }

  TfraRomExplorer = class(TFrame)
    Label1: TLabel;
    panGraph: TPanel;
  private
    cxp: TCompilerBase;
    pic: TPicCore;
    blockUse: array of TRomBlock;
    blockSta: array of TRomBlock;
    procedure DrawBlock(const marcoRam: TRect; ancMargenDir: integer; dirIni,
      dirFin, PageSize: word);
    procedure DrawBlockTxt(const marcoRam: TRect; ancMargenDir: integer;
      dirIni, dirFin, PageSize: word; lbl: string);
    procedure DrawROMpage(const marcoRam: TRect; pag: TPICFlashPage;
      selected: boolean);
    procedure DibBar(const x1, x2: integer; y1, y2: integer; lbl: string);
    procedure panGraphPaint(Sender: TObject);
    procedure SplitInStateROM(dir1, dir2: word);
    procedure SplitInUsedROM(dir1, dir2: word);
  public
    procedure SetCompiler(cxp0: TCompilerBase);
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
{ TfraRamExplorer }
procedure TfraRomExplorer.SplitInUsedROM(dir1, dir2: word);
{Explora la memoria ROM, entre las direcciones dir1, y dir2, y almacena los bloques
encontrados (de acuerdo a si están usados o no), en el arreglo "blockUse".
}
var
  n: Integer;
  i, pos1, pos2: Word;
  used: boolean;
begin
  setlength(blockUse, 1);  //abre un bloque
  n := high(blockUse);  //índice actual
  for i := dir1 to dir2 do begin
    if i = dir1 then begin
      //Define el bloque inicial
      used := pic.flash[i].used;
      pos1 := i;
      pos2 := i;
    end else begin
      if pic.flash[i].used = used then begin
        //Es del bloque anterior
        pos2 := i;   //actualiza límite
      end else begin
        //Es otro tipo de bloque.
        //Cierra el anterior
        blockUse[n].add1 := pos1;
        blockUse[n].add2 := pos2;
        blockUse[n].used:= used;
        n := n + 1;
        setlength(blockUse, n+1);
        //Define nuevo blqoue
        used := pic.flash[i].used;
        pos1 := i;
        pos2 := i;
      end;
    end;
  end;
  //Cierra el último bloque
  blockUse[n].add1 := pos1;
  blockUse[n].add2 := pos2;
  blockUse[n].used:= used;
end;
procedure TfraRomExplorer.SplitInStateROM(dir1, dir2: word);
{Explora la memoria ROM, entre las direcciones dir1, y dir2, y almacena los bloques
encontrados (de acuerdo a si están implementados o no), en el arreglo "blockSta".
}
var
  MaxAddrFlash: Word;
begin
  MaxAddrFlash := pic.MaxFlash - 1;  //Dirección final implementada
  {La memoria Flash solo puede estar implementada o no. Se asume que toda la memoria
  por debajo de MaxAddrFlash, está implementada.}
  if MaxAddrFlash=0 then begin
    //No se aplica MaxFlash.
    setlength(blockSta, 1);  //abre un bloque
    blockSta[0].add1 := dir1;
    blockSta[0].add2 := dir2;
    blockSta[0].implemented:= true;
  end else if MaxAddrFlash >= dir2 then begin
    //Estamos dentro del rango de Flash implementada
    setlength(blockSta, 1);  //abre un bloque
    blockSta[0].add1 := dir1;
    blockSta[0].add2 := dir2;
    blockSta[0].implemented:= true;
  end else if MaxAddrFlash >= dir1 then begin
    //Esta página contiene parte de la memoria implementada
    setlength(blockSta, 2);  //abre dos bloques
    blockSta[0].add1 := dir1;
    blockSta[0].add2 := MaxAddrFlash;
    blockSta[0].implemented:= true;
    //Parte no implementada
    blockSta[1].add1 := MaxAddrFlash+1;
    blockSta[1].add2 := dir2;
    blockSta[1].implemented:= false;
  end else begin
    //Toda la página está no implementada
    setlength(blockSta, 1);  //abre un bloque
    blockSta[0].add1 := dir1;
    blockSta[0].add2 := dir2;
    blockSta[0].implemented:= false;
  end;
end;
procedure TfraRomExplorer.DibBar(const x1, x2: integer; y1, y2: integer;
                                 lbl: string);
//Dibuja una barra, en la posición: x1, x2, y, con altura "alto".
var
  altTxt, ancho, ancTxt, xt, yt, alto: Integer;
  ARect: TRect;
  TextStyle: TTextStyle;
  cv: TCanvas;
begin
  alto := y2 - y1;
  cv := panGraph.Canvas;
  if (cv.Brush.Color = clNone) and (cv.Brush.Style = bsSolid) then begin
    //Sin fondo
    cv.Frame(x1, y1, x2, y2+1);  //Corrige y2, porque Rectangle, dibuja hasta un pincel antes
  end else begin
    //Con fondo
    cv.Rectangle(x1, y1, x2, y2+1); //Corrige y2, porque Rectangle, dibuja hasta un pincel antes
  end;
  //////// Escribe etiqueta centrada /////////////
  ancho := x2 - x1;
  altTxt := cv.TextHeight(lbl);
  ancTxt := cv.TextWidth(lbl);
  ARect.Left := x1;
  ARect.Right := x2;
  ARect.Top := y1;
  ARect.Bottom := y2;
  if ancTxt> ancho then
     xt := x1  //no centra
  else
     xt := x1 + ancho div 2 -  ancTxt div 2;  //centra
  yt := y1 + alto div 2 - altTxt div 2;
//  cv.Pen.Color := clred;
  TextStyle := cv.TextStyle;
  TextStyle.EndEllipsis := true;
  TextStyle.SingleLine := false;
  cv.Brush.Style := bsClear;  //texto sin fondo
  cv.TextRect(Arect, xt, yt, lbl, TextStyle);
end;
procedure TfraRomExplorer.DrawBlockTxt(const marcoRam: TRect;
                                 ancMargenDir: integer;
                                 dirIni, dirFin, PageSize: word; lbl: string);
{Dibuja un bloque de un banco de RAM (definida en el área "marcoRam"), pone etiqueta
descriptiva y pinta con color indicativo.
El bloque a dibujar, empieza en la dirección "dirIni" y termina en "dirFin".}
var
  etiqIni, etiqFin: String;
  altTxt: integer;  //Ancho de margen para las etiquetas de dirección.
  cv: TCanvas;
  x1, x2: LongInt;
  altoByte: Double;
  y1, y2, alto: integer;
  BankMask: Word;
begin
  BankMask := PageSize-1;  //Máscara para el banco: $7F (Mid-range) o $1F (Baseline)

  x1 := marcoRam.Left;
  x2 := marcoRam.Right;
  altoByte := (marcoRam.Bottom - marcoRam.Top)/PageSize;
  y1 := round(marcoRam.Top + (dirIni and BankMask) * altoByte);
  y2 := round(marcoRam.Top + ((dirFin and BankMask)+1) * altoByte);
  alto := y2 - y1;

  cv := panGraph.Canvas;
  //Convierte direcciones a texto y1
  etiqIni := IntToHex(dirIni, 3);
  etiqFin := IntToHex(dirFin, 3);
  //Dibuja barra de fondo
  DibBar(x1+ancMargenDir, x2, y1, y2, lbl);
  //Dibuja etiquetas de dirección
  if ancMargenDir <> 0 then begin
    altTxt := cv.TextHeight(etiqFin);
    if (alto>0.8*altTxt)  then begin
      cv.TextOut(x1, y1, etiqIni);
    end;
    if (alto>1.6*altTxt)  then begin
      cv.TextOut(x1, y2-altTxt, etiqFin);
    end;
  end;
end;
procedure TfraRomExplorer.DrawBlock(const marcoRam: TRect;
                                 ancMargenDir: integer;
                                 dirIni, dirFin, PageSize: word);
{Similar a DrawBlockTxt(), pero no pone las etiquetas de dirección, ni la etiqueta
central.}
var
  cv: TCanvas;
  x1, x2: LongInt;
  altoByte: Double;
  y1, y2: integer;
  BankMask: Word;
begin
  BankMask := PageSize-1;  //Máscara para el banco: $7F (Mid-range) o $1F (Baseline)

  x1 := marcoRam.Left;
  x2 := marcoRam.Right;
  altoByte := (marcoRam.Bottom - marcoRam.Top)/PageSize;
  y1 := round(marcoRam.Top + (dirIni and BankMask) * altoByte);
  y2 := round(marcoRam.Top + ((dirFin and BankMask)+1) * altoByte);
  //Dibuja barra de fondo
  cv := panGraph.Canvas;
  cv.Rectangle(x1+ancMargenDir,
               y1, x2, y2+1);  //Corrige y2, porque Rectangle, dibuja hasta un pincel antes
end;
procedure TfraRomExplorer.DrawROMpage(const marcoRam: TRect; pag: TPICFlashPage;
                                      selected: boolean);
{Dibuja la página de ROM completa, en el área "marcoRam", separando por bloques
de acuerdo al campo "state" }
var
  cv: TCanvas;
  ancMargenDir, i: Integer;
  lbl: String;
  tmp: Word;
begin
  cv := panGraph.Canvas;
  //Calcula el ancho de las etqiuetas
  ancMargenDir := cv.TextWidth('XXX');
  if ancMargenDir>(marcoRam.Right - marcoRam.Left)*0.5 then begin
    ancMargenDir := 0;
  end;
  //Limpia fondo
//  DrawBlock(marcoRam, ancMargenDir, blockSta[i].add1, blockSta[i].add2);  //dibuja;

  //Dibuja primero los bloques de memoria usadas
  SplitInUsedROM(pag.AddrStart, pag.AddrEnd);
  cv.Pen.Color := $80FF80;
  cv.Brush.Style := bsSolid;
  for i:=0 to high(blockUse) do begin
    if blockUse[i].used then begin
      cv.Brush.Color := $80FF80;
      DrawBlock(marcoRam, ancMargenDir,
                blockUse[i].add1, blockUse[i].add2, pic.PICPAGESIZE);  //dibuja;
    end;
  end;
  //Verifica si está seleccionada
  if selected then begin
    cv.Pen.Width := 2;
    cv.Pen.Color := clBlue;
    cv.Frame(marcoRam.Left+ancMargenDir-1, marcoRam.Top-1, marcoRam.Right+2, marcoRam.Bottom+3);
  end;
  cv.Pen.Width := 1;
  cv.Pen.Color := clBlack;
  //Separa bloques
  SplitInStateROM(0+pag.AddrStart, pag.AddrEnd);
  // Dibuja los bloques
  for i:=0 to high(blockSta) do begin
    //Crea etiqueta
    if blockSta[i]. implemented then begin
//      cv.Brush.Color := clWhite;
      cv.Brush.Color := clNone;  //Transparente
      lbl := 'ROM';
    end else begin
      cv.Brush.Color := clGray;
      lbl := 'Uninplemented';
    end;
    tmp := pic.PICPAGESIZE;
    if blockSta[i].implemented then begin
        //Bloque implementado
        DrawBlockTxt(marcoRam, ancMargenDir,
                blockSta[i].add1, blockSta[i].add2, tmp, lbl);  //dibuja;
    end else begin
        //Bloque no implementado
        cv.Brush.Style := bsSolid;
        DrawBlockTxt(marcoRam, ancMargenDir,
                blockSta[i].add1, blockSta[i].add2, tmp, lbl);  //dibuja;
    end;
  end;

end;
procedure TfraRomExplorer.panGraphPaint(Sender: TObject);
var
  bordlat, ancPag, x0, i, separ, alto, bordSup, y0: Integer;
begin
  //Espaciado entre bancos
  if panGraph.width < 200 then begin
    //Ancho reducido
    separ := panGraph.width div 24;  //espacio lateral
    bordlat := panGraph.width div 24;
  end else begin
    //Ancho grande
    if (cxp.PICnPages=1) then begin
      //Es una sola página. Puede quedar mal proporcionada
      separ := panGraph.width div 4;  //espacio lateral
      bordlat := panGraph.width div 4;
    end else begin
      separ := panGraph.width div 18;  //espacio lateral
      bordlat := panGraph.width div 18;
    end;
  end;
  bordSup := panGraph.height div 15;  //espacio superior
  ancPag := (panGraph.width - bordlat * 2 - separ * (cxp.PICnPages-1)) div cxp.PICnPages;
  alto := panGraph.height - 2* bordSup;
//debugln('panGraph.width: %d bordLat: %d', [panGraph.width, bordlat]);
  x0 := bordlat;
  y0 := bordsup;
  for i:=0 to cxp.PICnPages-1 do begin
    DrawROMpage(Rect(x0, y0, x0+ancPag, y0+alto), cxp.PICPage(i), false);
    x0 := x0 + ancPag + separ;
  end;
end;
procedure TfraRomExplorer.SetCompiler(cxp0: TCompilerBase);
begin
  cxp := cxp0;
  pic := cxp0.picCore;
end;
constructor TfraRomExplorer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //self.OnPaint := @Frame1Paint;
  panGraph.OnPaint := @panGraphPaint;
end;
destructor TfraRomExplorer.Destroy;
begin

  inherited Destroy;
end;

end.

