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
    acGenStep: TAction;
    acGenStepIn: TAction;
    acGenSetPC: TAction;
    acGenExecHer: TAction;
    acGenRun: TAction;
    acGenPause: TAction;
    acGenSetBrkPnt: TAction;
    acGenClearCC: TAction;
    ActionList1: TActionList;
    Image1: TImage;
    ImageList32: TImageList;
    ImageList16: TImageList;
    Label1: TLabel;
    lstMessages: TListBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    mnSetPCHere: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    panRAM: TPanel;
    panROM: TPanel;
    panStatis: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    Splitter5: TSplitter;
    Splitter6: TSplitter;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    procedure acGenClearCCExecute(Sender: TObject);
    procedure acGenExecHerExecute(Sender: TObject);
    procedure acGenSetBrkPntExecute(Sender: TObject);
    procedure acGenStepExecute(Sender: TObject);
    procedure acGenStepInExecute(Sender: TObject);
    procedure acGenPauseExecute(Sender: TObject);
    procedure acGenResetExecute(Sender: TObject);
    procedure acGenRunExecute(Sender: TObject);
    procedure acGenSetPCExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    fraRamExp: TfraRamExplorer;
    fraRomExp: TfraRomExplorer;
    fraPicReg: TfraPicRegisters;
    fraRegWat: TfraRegWatcher;
    milsecRefresh: integer;   //Periodo de refresco en milisegunod
    nCyclesPerClk: integer;   //Número de ciclos a ejecutar por pasada
    procedure picExecutionMsg(message: string);
    procedure RefreshScreen(SetGridRow: boolean = true);
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
procedure TfrmDebugger.Timer1Timer(Sender: TObject);
var
  stopped: boolean;
begin
  if pic = nil then exit;
  pic.ExecNCycles(nCyclesPerClk, stopped);
  if stopped then begin
    acGenPauseExecute(self);
  end else begin
    RefreshScreen;
  end;
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
      cv.Brush.Color := clMenu;  // fondo marrón
      cv.Font.Color := clBlack;    // letra negra
      cv.Font.Style := [fsBold];   // negrita
    end else begin
      //Fila sin selección
      if pic.flash[aRow].used then begin
        cv.Brush.Color := clWhite;  //fondo blanco
      end else begin  //Dirección no usada
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
    //Breakpoint
    if pic.flash[aRow].breakPnt then begin
      ImageList16.Draw(cv, aRect.Left + 1, aRect.Top+2, 9);
    end;
    //Columna de marca
    if aRow = word(pic.PCH<<8) + pic.PCL then begin  //marca
       ImageList16.Draw(cv, aRect.Left + 10, aRect.Top+2, 3);
    end;
  end else if ACol = 2 then begin
    //Celda normal
    valp := pic.flash[aRow].value;
    pic.Decode(valp);   //decodifica instrucción
    txt := pic.Disassembler(true);
    //Escribe texto con alineación
    cv.TextOut(aRect.Left + 2, aRect.Top + 2, txt);
  end else if ACol = 3 then begin
    //Celda normal
//    valp := pic.flash[aRow].value;
//    pic.Decode(valp);   //decodifica instrucción
    txt := pic.flash[aRow].topLabel;
    //Escribe texto con alineación
    cv.TextOut(aRect.Left + 2, aRect.Top + 2, txt);
  end;
end;
procedure TfrmDebugger.RefreshScreen(SetGridRow: boolean = true);
{Refresca los paneles de la pantalla, con información actual del PIC}
var
  pc: word;
begin
  if SetGridRow then begin
    pc := pic.PCH<<8 + pic.PCL;
    StringGrid1.Row := pc;
  end;
  StringGrid1.Invalidate;  //Refersca posibles cambios
  fraPicReg.Refrescar;
  if fraRamExp.Visible then fraRamExp.panGraph.Invalidate;
  fraRegWat.Refrescar;
  StatusBar1.Panels[1].Text := 'Clock Cycles = ' + IntToStr(pic.nClck);
//  StatusBar1.Panels[1].Text := 'Time seconds = ' + FloatToStr(pic.nClck / pic.frequen);
  StatusBar1.Panels[2].Text := 'Time  = ' +
            FormatDateTime('hh:mm:ss.zzz', 4 * pic.nClck / pic.frequen / 86400);
end;
procedure TfrmDebugger.picExecutionMsg(message: string);
var
  i: Integer;
begin
  lstMessages.AddItem(message, nil);
  if lstMessages.Count>100 then begin
    //Limita la cantidad de mensajes
    lstMessages.Items.BeginUpdate;
    for i:=1 to 10 do begin
      lstMessages.Items.Delete(0);
    end;
    lstMessages.AddItem('Too many messages. STOP command sent.', nil);
    lstMessages.Items.EndUpdate;
    pic.CommStop := true;  //Manda comando para detener
  end;
end;
procedure TfrmDebugger.acGenResetExecute(Sender: TObject);
begin
  pic.Reset;
  Timer1.Enabled := false;
  acGenRun.Enabled := true;
  acGenPause.Enabled := false;
  RefreshScreen;
  lstMessages.AddItem('Resetting device.', nil);
end;
procedure TfrmDebugger.acGenRunExecute(Sender: TObject);
{Ejecuta el programa, desde la posición actual}
var
  stopped: boolean;
begin
  pic.CommStop := false;   //Por si acaso
  //Ejecuta la primera instrucción para pasar en caso de que haya Puntos de Interrupción
  pic.Exec;
  {Hace una primera ejecución, porque la primera ejecución del Timer, va a demorar.}
  pic.ExecNCycles(nCyclesPerClk, stopped);
  if stopped then begin
    //Bastó una sola pasada, para llegar a algún obstáculo
    RefreshScreen;
    exit;
  end;
  //Programa la ejecución temporizada
  Timer1.Enabled := true;
  acGenRun.Enabled := false;
  acGenPause.Enabled := true;
  RefreshScreen;
  lstMessages.AddItem('Running program.', nil);
end;
procedure TfrmDebugger.acGenPauseExecute(Sender: TObject);
{Detiene el programa en el punto actual.}
begin
  Timer1.Enabled := false;
  acGenRun.Enabled := true;
  acGenPause.Enabled := false;
  RefreshScreen;
  lstMessages.AddItem('Execution Paused.', nil);
end;
procedure TfrmDebugger.acGenSetPCExecute(Sender: TObject);
//Fija el puntero del programa en la instrucción seleccionad
begin
  if StringGrid1.Row=-1 then exit;
  pic.PCL := StringGrid1.Row and $FF;
  pic.PCH := StringGrid1.Row >> 8;
end;
procedure TfrmDebugger.acGenExecHerExecute(Sender: TObject);
{Ejecuta una instrucción hasta la dirección seleccionada.}
var
  pc: word;
begin
  if StringGrid1.Row=-1 then exit;
  pc := StringGrid1.Row;
  pic.ExecTo(pc);  //Ejecuta hasta la sgte. instrucción, salta el CALL
  RefreshScreen;
end;
procedure TfrmDebugger.acGenClearCCExecute(Sender: TObject);
{Reinica el contador de ciclos.}
begin
  pic.nClck := 0;
  RefreshScreen(false);
end;
procedure TfrmDebugger.acGenSetBrkPntExecute(Sender: TObject);
{Pone o quita un Punto de Interrupción en la posición indicada}
var
  pc: word;
begin
  if StringGrid1.Row=-1 then exit;
  pc := StringGrid1.Row;
  pic.ToggleBreakopint(pc);
  RefreshScreen(false);
end;
procedure TfrmDebugger.acGenStepExecute(Sender: TObject);
{Ejecuta una instrucción sin entrar a subrutinas}
var
  pc: word;
begin
  if pic.CurInstruction = CALL then begin
    pc := pic.PCH<<8 + pic.PCL;
    pic.ExecTo(pc+1);  //Ejecuta hasta la sgte. instrucción, salta el CALL
  end else begin
    pic.Exec();
  end;
  RefreshScreen;
end;
procedure TfrmDebugger.acGenStepInExecute(Sender: TObject);
{Ejecuta una isntrucción, entrando al código de las subrutinas.}
begin
  pic.Exec();
  RefreshScreen;
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
  pic.AddBreakopint(0);
  pic.OnExecutionMsg := @picExecutionMsg;
  acGenResetExecute(self);
  StatusBar1.Panels[0].Text := pic.Model + ' at ' + IntToStr(pic.frequen) + ' Hz';
  /////////////////////////////////////////////////////////////////////////////////
  ///// Calcula parámetros de refresco, para la ejecución en tiempo real //////////
  {La idea de la ejecución en tiempo real, es ejecutar un paquete de instrucciones
  (ciclos) por bloques y luego aprovechar el tiempo muerto que queda por haber ejecutado
  todas las instrucciones en menor tiempo.
  Si se fuera estricto en la simualción y tiempo real, se ejecutaría instrucción por
  instrucción aprovechar el tiempo muerto que queda, después de cada instrucción, ya que
  la PC, ejecuta cada instrucción PIC, en menos tiempo (al menos eso se espera).
  En pruebas con una PC Core i7 con 3.4GHz, se calculó que se podía ejecutar las
  instruccioes al menos 12 veces más rápido, para un dispositivo trabajando a 10MHz.}
  milsecRefresh := 200;   //Fija un periodo de refresco inicial
  Timer1.Interval := milsecRefresh;
  {Calcula cuántos ciclos debe ejecutar por refresco. Aún cuando el resultado de la
  fórmula sea exacto, la función ExecNCycles() usada para ejecutar un grupo de ciclos
  no siempre ejecuta los ciclos solicitados exactamente.}
  nCyclesPerClk := round(int64(pic.frequen) * milsecRefresh / 4000);
  /////////////////////////////////////////////////////////////////////////////////
  self.Show;
end;

end.

