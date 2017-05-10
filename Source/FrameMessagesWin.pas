{Implemnenta la ventana de mensajes}
unit FrameMessagesWin;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, Grids, Graphics,
  ExtCtrls, StdCtrls, Parser, UtilsGrilla, BasicGrilla;
type

  { TUtilGrillaFil2 }

  TUtilGrillaFil2 = class(TUtilGrillaFil)
    procedure grillaDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState); override;
  end;

  { TfraMessagesWin }
  TfraMessagesWin = class(TFrame)
    chkInform: TCheckBox;
    chkWarns: TCheckBox;
    chkErrors: TCheckBox;
    ImgMessages: TImageList;
    lblRAM: TLabel;
    lblROM: TLabel;
    lblSTACK: TLabel;
    panStatis: TPanel;
    Panel2: TPanel;
    grilla: TStringGrid;
    PanGrilla: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure chkErrorsChange(Sender: TObject);
    procedure chkInformChange(Sender: TObject);
    procedure chkWarnsChange(Sender: TObject);
    procedure PanGrillaResize(Sender: TObject);
    procedure panStatisPaint(Sender: TObject);
  private
    cxp: TCompiler;
    UtilGrilla: TUtilGrillaFil2;
    timeCnt: DWORD;
    nVis, nWar, nErr: Integer;
    usedRAM, usedROM, usedSTK: single;
    procedure FilterGrid;
    procedure CountMessages;
  public
    procedure GetFirstError(out msg: string; out filname: string; out row,
      col: integer);
    procedure InitCompilation(cxp0: TCompiler);
    procedure EndCompilation;
    procedure AddError(errTxt: string; fileName: string; row, col: integer);
    procedure AddInformation(infTxt: string);
    procedure AddWarning(warTxt: string; fileName: string; row, col: integer);
  public //Inicialización
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
    procedure SetLanguage(lang0: string);
  end;

implementation
{$R *.lfm}
const
  ROW_HEIGH = 19;
  //Constantes para identificar íconos
  ICO_INF = '0';
  ICO_WAR = '1';
  ICO_ERR = '2';
var
  //Variables para índice de columnas
  GCOL_ICO: integer;  //ícono
  GCOL_TXT: integer;  //texto a mostrar en la grilla
  GCOL_FILE: integer;  //archivo (incluyendo ruta)
  GCOL_ROW: integer;  //número de fila
  GCOL_COL: integer;  //número ed columna
  GCOL_MSG: integer;  //mensaje

{ TUtilGrillaFil2 }
procedure TUtilGrillaFil2.grillaDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  cv: TCanvas;           //referencia al lienzo
  atrib: integer;
begin
  cv := grilla.Canvas;  //referencia al Lienzo
  if gdFixed in aState then begin
    //Es una celda fija
    cv.Font.Color := clBlack;
    cv.Font.Style := [];
    cv.Brush.Color := clBtnFace;
    cv.FillRect(aRect);   //fondo
    DibCeldaTexto(aCol, aRow, aRect);
  end else begin
    //Es una celda común
    cv.Font.Color := TColor(PtrUInt(grilla.Objects[1, aRow]));
    if grilla.Objects[2, aRow]=nil then begin
      //Sin atributos
      cv.Font.Style := [];
    end  else begin
      //Hay atributos de texto
      atrib := PtrUInt(grilla.Objects[2, aRow]);
      if (atrib and 1) = 1 then cv.Font.Style := cv.Font.Style + [fsUnderline];
      if (atrib and 2) = 2 then cv.Font.Style := cv.Font.Style + [fsItalic];
      if (atrib and 4) = 4 then cv.Font.Style := cv.Font.Style + [fsBold];
    end;
    if OpResaltFilaSelec and EsFilaSeleccionada(aRow) then begin
      //Fila seleccionada. (Debe estar activada la opción "goRowHighligh", para que esto funcione bien.)
      cv.Brush.Color := clBtnFace;
    end else begin
      cv.Brush.Color := TColor(PtrUInt(grilla.Objects[0, aRow]));
    end;
    cv.FillRect(aRect);   //fondo
    if cols[aCol].tipo = ugTipIco then
      DibCeldaIcono(aCol, aRow, aRect)
    else
      DibCeldaTexto(aCol, aRow, aRect);
    // Dibuja ícono
{    if (aCol=0) and (aRow>0) then
      ImageList16.Draw(grilla.Canvas, aRect.Left, aRect.Top, 19);}
    //Dibuja borde en celda seleccionada
    if gdFocused in aState then begin
//      cv.Pen.Color := clRed;
//      cv.Pen.Style := psDot;
//      cv.Frame(aRect.Left, aRect.Top, aRect.Right-1, aRect.Bottom-1);  //dibuja borde
    end;
  end;
end;

{ TfraMessagesWin }
procedure TfraMessagesWin.AddInformation(infTxt: string);
var
  f: Integer;
begin
  f := grilla.RowCount;
  grilla.RowCount := f + 1;
  grilla.Cells[GCOL_ICO, f] := ICO_INF;  //ícono
  grilla.Cells[GCOL_TXT, f] := infTxt;
  grilla.Cells[GCOL_FILE, f] := '';
  grilla.Cells[GCOL_ROW, f] := '-1';
  grilla.Cells[GCOL_COL, f] := '-1';
  grilla.Cells[GCOL_MSG, f] := infTxt;
  grilla.RowHeights[f] := ROW_HEIGH;
  UtilGrilla.FijColorFondo(f, clWhite);  //Color de fondo de la fila
end;
procedure TfraMessagesWin.AddWarning(warTxt: string; fileName: string; row,
  col: integer);
var
  f: Integer;
begin
  f := grilla.RowCount;
  grilla.RowCount := f + 1;
  grilla.Cells[GCOL_ICO, f] := ICO_WAR;  //ícono
  grilla.Cells[GCOL_TXT, f] := ExtractFileNameOnly(fileName) +
    '['+ IntToStr(row) + ',' + IntToStr(col) + '] '+  ' Warning: ' + warTxt;
  grilla.Cells[GCOL_FILE, f] := fileName;
  grilla.Cells[GCOL_ROW, f] := IntToStr(row);
  grilla.Cells[GCOL_COL, f] := IntToStr(col);
  grilla.Cells[GCOL_MSG, f] := warTxt;   //El texto tal cual
  grilla.RowHeights[f] := ROW_HEIGH;
  UtilGrilla.FijColorFondo(f, clWhite);  //Color de fondo de la fila
//  FijColorTexto(f, );  //Color del texto de la fila
end;
procedure TfraMessagesWin.AddError(errTxt: string; fileName: string; row,
  col: integer);
var
  f: Integer;
begin
  f := grilla.RowCount;
  grilla.RowCount := f + 1;
  grilla.Cells[GCOL_ICO, f] := ICO_ERR;  //ícono
  grilla.Cells[GCOL_TXT, f] := ExtractFileNameOnly(fileName) +
    '['+ IntToStr(row) + ',' + IntToStr(col) + '] '+  ' Error: ' + errTxt;
  grilla.Cells[GCOL_FILE, f] := fileName;
  grilla.Cells[GCOL_ROW, f] := IntToStr(row);
  grilla.Cells[GCOL_COL, f] := IntToStr(col);
  grilla.Cells[GCOL_MSG, f] := errTxt;   //El texto tal cual
  grilla.RowHeights[f] := ROW_HEIGH;
  UtilGrilla.FijColorFondo(f, clWhite);  //Color de fondo de la fila
  UtilGrilla.FijColorTexto(f, clRed);  //Color del texto de la fila
end;
procedure TfraMessagesWin.chkInformChange(Sender: TObject);
begin
  FilterGrid;
end;
procedure TfraMessagesWin.chkErrorsChange(Sender: TObject);
begin
  FilterGrid;
end;
procedure TfraMessagesWin.chkWarnsChange(Sender: TObject);
begin
  FilterGrid;
end;
procedure TfraMessagesWin.PanGrillaResize(Sender: TObject);
begin
  grilla.ColWidths[2] := PanGrilla.Width-50;
end;
procedure TfraMessagesWin.CountMessages;
var
  f: Integer;
begin
  //Cuenta advertencias, errores, y filas visibles
  nVis := 0;
  nWar := 0;
  nErr := 0;
  for f:=1 to grilla.RowCount -1 do begin
    if grilla.Cells[GCOL_ICO, f] = ICO_WAR then Inc(nWar);
    if grilla.Cells[GCOL_ICO, f] = ICO_ERR then Inc(nErr);
    if grilla.RowHeights[f] > 0 then inc(nVis);
  end;
end;
procedure TfraMessagesWin.GetFirstError(out msg: string; out filname: string;
                                        out row, col: integer);
{Devuelve información sobre el primer error de la lista de errores.
Si el error no tiene información de ubicación, devuelve -1 en "row" y "col".
Si no encuentra algún error, devuelve cadena vacía en "msg".}
var
  f: Integer;
begin
  for f:=1 to grilla.RowCount -1 do begin
    if grilla.Cells[GCOL_ICO, f] = ICO_ERR then begin
      msg := grilla.Cells[GCOL_MSG, f];
      filname := grilla.Cells[GCOL_FILE, f];
      TryStrToInt(grilla.Cells[GCOL_ROW, f], row);
      TryStrToInt(grilla.Cells[GCOL_COL, f], col);
      exit;
    end;
  end;
  //No encontró
  msg := '';
end;
procedure TfraMessagesWin.FilterGrid;
var
  f: integer;
  icoId: String;
begin
  grilla.BeginUpdate;
  for f:=1 to grilla.RowCount -1 do begin
    icoId := grilla.Cells[GCOL_ICO, f];
    case icoId of
    ICO_INF: begin
      if chkInform.Checked then grilla.RowHeights[f] := ROW_HEIGH
      else grilla.RowHeights[f] := 0;
    end;
    ICO_WAR: begin
      if chkWarns.Checked then grilla.RowHeights[f] := ROW_HEIGH
      else grilla.RowHeights[f] := 0;
    end;
    ICO_ERR: begin
      if chkErrors.Checked then grilla.RowHeights[f] := ROW_HEIGH
      else grilla.RowHeights[f] := 0;
    end;
    end;
  end;
  CountMessages;
  if nVis = 0 then begin
    grilla.Visible := false;
  end else begin
    grilla.Visible := true;
    grilla.Row := PrimeraFilaVis(grilla);
  end;
  grilla.EndUpdate;
end;
procedure TfraMessagesWin.InitCompilation(cxp0: TCompiler);
begin
  cxp := cxp0;   //Guarda referencia
  grilla.RowCount := 1;   //Limpia Grilla
  cxp.OnWarning := @AddWarning;  //Inicia evento
  cxp.OnError := @AddError;
  timeCnt:=GetTickCount64;
  AddInformation('Starting Compilation ...');
//  chkInform.Checked := true;
//  chkWarns.Checked := true;
//  chkErrors.Checked := true;
end;
procedure TfraMessagesWin.EndCompilation;
var
  infWar, infErr: String;
begin
  //Construye información adicional
  CountMessages;
  if nWar = 1 then begin
    infWar := '1 warning'
  end else begin
    infWar := IntToStr(nWar) + ' warnings';
  end;
  if nErr = 1 then begin
    infErr := '1 error'
  end else begin
    infErr := IntToStr(nErr) + ' errors';
  end;
  AddInformation('Compiled in: ' + IntToStr(GetTickCount64-timeCnt) + ' msec. <<' +
                 infWar + ', ' + infErr + '>>');
  //Actualiza estadísticas de uso
  if nErr=0 then begin
    //No hay error
    cxp.GetResourcesUsed(usedRAM, usedROM, usedSTK);
    panStatis.Invalidate;
    AddInformation(cxp.RAMusedStr + ', ' + cxp.FLASHusedStr) ;
  end else begin
    //Hubo errores
    usedRAM:=0;
    usedROM:=0;
    usedSTK:=0;
    panStatis.Invalidate;
  end;
  FilterGrid;
  //Posiciona al final
  if grilla.RowCount>1 then begin
    grilla.Row := grilla.RowCount -1;
  end;
end;
procedure TfraMessagesWin.panStatisPaint(Sender: TObject);
var
  cv: TCanvas;
  procedure Barra(x0, y0: integer; alt: integer; porc: Single);
  var
    alt2, dif: Integer;
    n: Int64;
  begin
    if alt<15 then exit;
    if alt>120 then alt := 100;
    //Dibuja fondo
    {$ifdef UNIX}
    cv.Brush.Color := clForm;
    {$else}
    cv.Brush.Color := clMenu;
    {$endif}
    cv.FillRect(x0, y0, x0 + 20, y0 +alt);
    //Dibuja barra
    n := round(porc*100);
    cv.Pen.Color := clGreen;
    if n < 40 then begin
      cv.Brush.Color := clGreen;
    end else if n < 80 then begin
      cv.Brush.Color := clYellow;
    end else begin
      cv.Brush.Color := clRed;
    end;
    alt2 := Round(alt*porc);
    dif := alt-alt2;
    cv.FillRect(x0, y0 + dif , x0 + 20, y0 + alt2 + dif);
    //Borde
    cv.Pen.Color := clGray;
    cv.Frame(x0, y0, x0 + 20, y0 +alt);
    cv.Frame(x0-2, y0-2, x0 + 22, y0 +alt+2);
    //Texto
    cv.Brush.Style := bsClear;
    cv.Font.Bold := true;
    if n<10 then begin
      cv.TextOut(x0+2, y0 + alt div 2 - 10, IntToStr(n)+'%');
    end else if n < 100 then begin
      cv.TextOut(x0-1, y0 + alt div 2 - 10, IntToStr(n)+'%');
    end else begin
      cv.TextOut(x0-3, y0 + alt div 2 - 10, IntToStr(n)+'%');
    end;
  end;
begin
  cv := panStatis.Canvas;
  Barra(lblRAM.Left + 5, lblRAM.Top + 20, panStatis.Height-35, usedRAM);
  Barra(lblROM.Left + 5, lblROM.Top + 20, panStatis.Height-35, usedROM);
  Barra(lblSTACK.Left+ 5, lblSTACK.Top + 20, panStatis.Height-35, usedSTK);
end;
//Inicialización
constructor TfraMessagesWin.Create(AOwner: TComponent);
var
  enc: TugGrillaCol;
begin
  inherited Create(AOwner);
  //Configura Grilla de mensajes
  UtilGrilla:= TUtilGrillaFil2.Create(grilla);
  grilla.FixedCols := 0;
  UtilGrilla.IniEncab;
  ///////// Definición de columnas //////////
  UtilGrilla.AgrEncabNum('' , 30).visible := false;  //Columna fija
  enc := UtilGrilla.AgrEncabIco('Ícono'  ,  24); //Ícono centrado
  GCOL_ICO := enc.idx;
  enc := UtilGrilla.AgrEncabTxt('Texto', 500) ;
  GCOL_TXT := enc.idx;
  enc := UtilGrilla.AgrEncabTxt('Archivo', 120);
  GCOL_FILE := enc.idx;
  enc.visible := false;
  enc := UtilGrilla.AgrEncabNum('Fila'   ,  20);
  GCOL_ROW := enc.idx;
  enc.visible := false;
  enc := UtilGrilla.AgrEncabNum('Columna',  20);
  GCOL_COL := enc.idx;
  enc.visible := false;
  enc := UtilGrilla.AgrEncabNum('Mensaje',  20);
  GCOL_MSG := enc.idx;
  enc.visible := false;
  /////////////////////////////////////////////
  UtilGrilla.FinEncab;
  UtilGrilla.OpDimensColumnas := true;
  UtilGrilla.OpDimensColumnas := true;
  UtilGrilla.ImageList := ImgMessages;  //Íconos a usar
  grilla.RowHeights[0] := 0;  //oculta envabezado
  grilla.Visible := false;
end;
destructor TfraMessagesWin.Destroy;
begin
  UtilGrilla.Destroy;
  inherited Destroy;
end;
procedure TfraMessagesWin.SetLanguage(lang0: string);
begin
  case lowerCase(lang0) of
  'en': begin
    PanGrilla.Caption := '<< No messages >>';
  end;
  'es': begin
    PanGrilla.Caption := '<< Sin mensajes >>';
  end;
  end;
end;

end.
