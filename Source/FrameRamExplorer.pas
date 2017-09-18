unit FrameRamExplorer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, StdCtrls, LCLProc,
  LCLIntf, LCLType, ExtCtrls, Buttons, Pic16Utils;
type
  //Define a un bloque de RAM, que servirá para dibujo
  TRamBlock = record
    add1, add2: word;   //direcciones de memoria
    blkType: TPIC16CellState;   //tipo de blqoue
    mapped: boolean;        //indica si el bloque está mapeado
  end;

  { TfraRamExplorer }
  TfraRamExplorer = class(TFrame)
    Label1: TLabel;
    panGraph: TPanel;
    panTitle: TPanel;
    SpeedButton1: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
  private
    blockSta: array of TRamBlock;
    blockMap: array of TRamBlock;
    procedure SepararEnBloquesEstado(dir1, dir2: word);
    procedure SepararEnBloquesMapeado(dir1, dir2: word);
    procedure DibBancoRAM(const marcoRam: TRect; bnk: TRAMBank; selected: boolean);
    procedure DibBar(const x1, x2: integer; y1, y2: integer; lbl: string);
    procedure DibBloque(const marcoRam: TRect; ancMargenDir: integer; dirIni,
      dirFin: integer; lbl: string);
    procedure panGraphPaint(Sender: TObject);
  public
    pic: TPIC16;
    OnCloseFrame: procedure of object;   //Evento de cierre
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
{ TfraRamExplorer }
procedure TfraRamExplorer.SpeedButton1Click(Sender: TObject);
begin
  if OnCloseFrame<>nil then OnCloseFrame;
end;
procedure TfraRamExplorer.SepararEnBloquesEstado(dir1, dir2: word);
{Explora la memoria RAM, entre las direcciones dir, y dir2, y almacena los bloques
encontrados (de distinto campo "state"), en el arreglo "blockSta".
}
var
  n: Integer;
  i, pos1, pos2: Word;
  tipBloque: TPIC16CellState;
begin
  setlength(blockSta, 1);  //abre un bloque
  n := high(blockSta);  //índice actual
  for i := dir1 to dir2 do begin
    if i = dir1 then begin
      //Define el bloque inicial
      tipBloque := pic.ram[i].state;
      pos1 := i;
      pos2 := i;
    end else begin
      if pic.ram[i].state = tipBloque then begin
        //Es del bloque anterior
        pos2 := i;   //actualiza límite
      end else begin
        //Es otro tipo de bloque.
        //Cierra el anterior
        blockSta[n].add1 := pos1;
        blockSta[n].add2 := pos2;
        blockSta[n].blkType := tipBloque;
        n := n + 1;
        setlength(blockSta, n+1);
        //Define nuevo blqoue
        tipBloque := pic.ram[i].state;
        pos1 := i;
        pos2 := i;
      end;
    end;
  end;
  //Cierra el último bloque
  blockSta[n].add1 := pos1;
  blockSta[n].add2 := pos2;
  blockSta[n].blkType := tipBloque;
end;
procedure TfraRamExplorer.SepararEnBloquesMapeado(dir1, dir2: word);
{Explora la memoria RAM, entre las direcciones dir, y dir2, y almacena los bloques
encontrados (de acuerdo a si están mapeados), en el arreglo "blockMap".
}
var
  n: Integer;
  i, pos1, pos2: Word;
  mapeado: boolean;
begin
  setlength(blockMap, 1);  //abre un bloque
  n := high(blockMap);  //índice actual
  for i := dir1 to dir2 do begin
    if i = dir1 then begin
      //Define el bloque inicial
      mapeado := (pic.ram[i].mappedTo<>nil);
      pos1 := i;
      pos2 := i;
    end else begin
      if (pic.ram[i].mappedTo<>nil) = mapeado then begin
        //Es del bloque anterior
        pos2 := i;   //actualiza límite
      end else begin
        //Es otro tipo de bloque.
        //Cierra el anterior
        blockMap[n].add1 := pos1;
        blockMap[n].add2 := pos2;
        blockMap[n].mapped:= mapeado;
        n := n + 1;
        setlength(blockMap, n+1);
        //Define nuevo blqoue
        mapeado := (pic.ram[i].mappedTo<>nil);
        pos1 := i;
        pos2 := i;
      end;
    end;
  end;
  //Cierra el último bloque
  blockMap[n].add1 := pos1;
  blockMap[n].add2 := pos2;
  blockMap[n].mapped:= mapeado;
end;

procedure TfraRamExplorer.DibBar(const x1, x2: integer; y1, y2: integer;
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
  cv.Rectangle(x1, y1, x2, y2+1);  //Corrige y2, porque Rectangle, dibuja hasta un pincel antes
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
procedure TfraRamExplorer.DibBloque(const marcoRam: TRect;
                                 ancMargenDir: integer;
                                 dirIni, dirFin: integer; lbl: string);
{Dibuja un bloque de un banco de RAM (definida en el área marcoRam), pone etiqueta
descriptiva y pinta con color indicativo.
El bloque a dibujar, empieza en la dirección "dirIni" y termina en "dirFin".
Se asume que todo el alto del banco RAM, tiene $80 bytes}
var
  etiqIni, etiqFin: String;
  altTxt: integer;  //Ancho de margen para las etiquetas de dirección.
  cv: TCanvas;
  x1, x2: LongInt;
  altoByte: Double;
  y1, y2, alto: integer;
begin
  x1 := marcoRam.Left;
  x2 := marcoRam.Right;
  altoByte := (marcoRam.Bottom - marcoRam.Top)/$80;
  y1 := round(marcoRam.Top + (dirIni and $7F) * altoByte);
  y2 := round(marcoRam.Top + ((dirFin and $7F)+1) * altoByte);
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
procedure TfraRamExplorer.DibBancoRAM(const marcoRam: TRect; bnk: TRAMBank;
                                 selected: boolean);
{Dibuja el banco de RAM completo, en el área "marcoRam", separando por bloques
de acuerdo al campo "state" }
var
  i, ancMargenDir, j: integer;
  cv: TCanvas;
  lbl: String;
  tarAdd, tarBnk: Word;
begin
  cv := panGraph.Canvas;
  //Calcula el ancho de las etqiuetas
  ancMargenDir := cv.TextWidth('XXX');
  if ancMargenDir>(marcoRam.Right - marcoRam.Left)*0.5 then begin
    ancMargenDir := 0;
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
  SepararEnBloquesEstado(0+bnk.AddrStart, $7F+bnk.AddrStart);
  // Dibuja los bloques
  for i:=0 to high(blockSta) do begin
    //Crea etiqueta
    case blockSta[i].blkType of
    cs_impleSFR: begin
  //    cv.Brush.Color := $FF9090;
      cv.Brush.Color := clWhite;
      lbl := 'SFR';
    end;
    cs_impleGPR: begin
      cv.Brush.Color := clWhite;
      lbl := 'GPR';
    end;
    cs_unimplem: begin
      cv.Brush.Color := clGray;
      lbl := 'Uninplemented';
    end;
    end;
    if blockSta[i].blkType = cs_impleGPR then begin
        //Divide el bloque, para ver zonas mapeadas
        SepararEnBloquesMapeado(blockSta[i].add1, blockSta[i].add2);
        for j:=0 to high(blockMap) do begin
          if blockMap[j].mapped then begin
             //Calcual en donde se está mapeado
             tarAdd := pic.ram[blockMap[j].add1].mappedTo^.addr;
             tarBnk :=  tarAdd >> 7;
             lbl := 'Mapped in bank'+ IntToStr(tarBnk);
             cv.Brush.Style := bsDiagCross;
          end else begin
             lbl := 'GPR';
             cv.Brush.Style := bsSolid;
          end;
          DibBloque(marcoRam, ancMargenDir,
                    blockMap[j].add1, blockMap[j].add2, lbl);  //dibuja;
        end;
    end else begin
        cv.Brush.Style := bsSolid;
        DibBloque(marcoRam, ancMargenDir,
                blockSta[i].add1, blockSta[i].add2, lbl);  //dibuja;
    end;
  end;
end;
procedure TfraRamExplorer.panGraphPaint(Sender: TObject);
var
  bordlat, ancPag, x0, i, separ, alto, bordSup, y0: Integer;
  bnkSel: byte;
begin
  if pic = nil then exit;
  ////////////////////////
  //Espaciado entre bancos
  if width < 300 then begin
    separ := panGraph.width div 24;  //espacio lateral
    bordlat := panGraph.width div 24;
  end else begin
    separ := panGraph.width div 18;  //espacio lateral
    bordlat := panGraph.width div 18;
  end;
  bordSup := (panGraph.height) div 15;  //espacio superior
  ancPag := (panGraph.width - bordlat * 2 - separ * (pic.NumBanks-1)) div pic.NumBanks;
  alto := (panGraph.height) - 2* bordSup;
//debugln('panGraph.width: %d bordLat: %d', [panGraph.width, bordlat]);
  x0 := bordlat;
  y0 := bordsup;
  bnkSel := pic.STATUS >> 5;
  for i:=0 to pic.NumBanks-1 do begin
    DibBancoRAM(Rect(x0, y0, x0+ancPag, y0+alto), pic.banks[i], i = bnkSel);
    x0 := x0 + ancPag + separ;
  end;
end;
constructor TfraRamExplorer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //self.OnPaint := @Frame1Paint;
  panGraph.OnPaint := @panGraphPaint;
end;
destructor TfraRamExplorer.Destroy;
begin

  inherited Destroy;
end;

end.

