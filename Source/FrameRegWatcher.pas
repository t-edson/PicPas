unit FrameRegWatcher;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Grids, ExtCtrls, StdCtrls,
  Buttons, Graphics, LCLType, Pic16Utils, MisUtils, CibGrillas;
type

  { TfraRegWatcher }

  TfraRegWatcher = class(TFrame)
    grilla: TStringGrid;
    Label1: TLabel;
    panTitle: TPanel;
    SpeedButton1: TSpeedButton;
  private
    UtilGrilla: TGrillaEdicFor;
    function FilaEstaVacia(f: integer): boolean;
    procedure grillaKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RefrescarPorDireccion(f: integer);
    procedure RefrescarPorNombre(f: integer);
    procedure UtilGrillaFinEditarCelda(var eveSal: TEvSalida; col,
      fil: integer; var ValorAnter, ValorNuev: string);
    procedure UtilGrillaKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    function UtilGrillaLeerColorFondo(col, fil: integer): TColor;
  public
    pic: TPIC16;
    procedure AddWatch(varName: string);
    procedure Refrescar;
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
const
  COLDIR = 1;  //Columna  de dirección
  COLNOM = 2;  //Columna de nombre
  COLVAL = 3;  //Columna de valor
{ TfraRegWatcher }
procedure TfraRegWatcher.Refrescar;
//Refresca el valor de los registros, en base a la dirección de memoria que tengan
var
  f: Integer;
begin
  if not Visible then exit;
  if pic = nil then exit;
  grilla.BeginUpdate;
  for f := 1 to grilla.RowCount-1 do begin
      grilla.Objects[1, f] := Tobject(Pointer(0));  //inicia con texto color negro
      RefrescarPorDireccion(f);
  end;
  grilla.EndUpdate();
end;
procedure TfraRegWatcher.RefrescarPorDireccion(f: integer);
{Refresca la fila f de la grilla, a partir del campo de dirección }
var
  addrStr: String;
  addr: Longint;
begin
  if pic = nil then exit;
  addrStr := grilla.Cells[COLDIR,f];
  if trim(addrStr) = '' then begin
     //No hay dato
     grilla.Cells[COLNOM,f] := '';
     grilla.Cells[COLVAL,f] := '';
  end else begin
    //Hay dirección
    if addrStr[1] = '$' then begin
       if not TryStrToInt('$'+copy(addrSTr,2,10), addr) then begin
          grilla.Cells[COLNOM,f] := '#error';
          grilla.Cells[COLVAL,f] := '#error';
          exit;
       end;
    end else begin
      //Debe ser decimal
      if not TryStrToInt(addrSTr, addr) then begin
         grilla.Cells[COLNOM,f] := '#error';
         grilla.Cells[COLVAL,f] := '#error';
         exit;
      end;
    end;
    if (addr<0) or (addr>$1FF) then begin
       grilla.Cells[COLNOM,f] := '#error';
       grilla.Cells[COLVAL,f] := '#error';
       exit;
    end;
    grilla.Cells[COLNOM,f] := pic.ram[addr].name;
    if grilla.Cells[COLVAL,f] <> '$'+IntToHex(pic.ram[addr].value, 2) then begin
       //hubo cambio
       grilla.Objects[1, f] := Tobject(Pointer(255));  //Pone color
//       grilla.Cells[0,f] := 'C';   //indica que hubo cambio
       grilla.Cells[COLVAL,f] := '$'+IntToHex(pic.ram[addr].value, 2);
    end;
  end;
end;
procedure TfraRegWatcher.RefrescarPorNombre(f: integer);
{Refresca la fila f de la grilla, a partir del campo de Nombre }
var
  nameStr: String;
  addr, i: integer;
begin
  if pic = nil then exit;
  nameStr := UpCase(grilla.Cells[COLNOM,f]);
  if trim(nameStr) = '' then begin
     //No hay dato
     grilla.Cells[COLDIR,f] := '';
     grilla.Cells[COLVAL,f] := '';
  end else begin
    //Hay nombre
    addr := -1;
    for i:=0 to PIC_MAX_RAM-1 do begin
      if UpCase(pic.ram[i].name) = nameStr then begin
        addr := i;
        break;
      end;
    end;
    if addr=-1 then begin  //No encontrado
       grilla.Cells[COLDIR,f] := '';
       grilla.Cells[COLVAL,f] := '#error';
       exit;
    end;
    grilla.Cells[COLDIR,f] := '$'+IntToHex(addr,3);
    grilla.Cells[COLVAL,f] := '$'+IntToHex(pic.ram[addr].value, 2);
  end;
end;
procedure TfraRegWatcher.UtilGrillaFinEditarCelda(var eveSal: TEvSalida; col,
  fil: integer; var ValorAnter, ValorNuev: string);
begin
  if eveSal = evsTecEscape then exit;
//  MsgBox('Editado: %d, %d', [fil, col]);
  if col=COLDIR then begin
     //Se editó la dirección
     grilla.Cells[col, fil] := ValorNuev;  //adelanta la escritura
     RefrescarPorDireccion(fil);
  end;
  if col=COLNOM then begin
     //Se editó el nombre
     grilla.Cells[col, fil] := ValorNuev;  //adelanta la escritura
     RefrescarPorNombre(fil);
  end;

end;
function TfraRegWatcher.FilaEstaVacia(f: integer): boolean;
begin
  Result := (grilla.cells[COLDIR, f]='') and
            (grilla.cells[COLNOM, f]='') and
            (grilla.cells[COLVAL, f]='');
end;
procedure TfraRegWatcher.grillaKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_UP) and                     //Se presiona flecha arriba
     (grilla.Row = grilla.RowCount-2) and  //Y pasó a la penúltima fila
     (FilaEstaVacia(grilla.RowCount-1))    //y la que sigue está vacía
  then begin
     grilla.RowCount := grilla.RowCount -1;
//     MsgBox('asdsad');
  end;
end;
procedure TfraRegWatcher.UtilGrillaKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DOWN) and                    //Se presiona flecha abajo
     (grilla.Row = grilla.RowCount-1) and   //Y es la última fila
     (not FilaEstaVacia(grilla.Row))
  then begin
     grilla.RowCount := grilla.RowCount + 1;  //agrega fila
  end;
end;
function TfraRegWatcher.UtilGrillaLeerColorFondo(col, fil: integer): TColor;
begin
  Result := clWhite;
end;
procedure TfraRegWatcher.AddWatch(varName: string);
{Agrega una variable para vigilar}
var
  f: Integer;
begin
  f := grilla.RowCount-1;  //última fila
  if not FilaEstaVacia(f) then begin
    //Hay que agregar una fila
    grilla.RowCount := grilla.RowCount + 1;
    f := grilla.RowCount-1;
  end;
  grilla.Cells[2,f] := varName;
  RefrescarPorNombre(f);
end;
constructor TfraRegWatcher.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //grilla.Options := grilla.Options + [goEditing, goColSizing];
  UtilGrilla := TGrillaEdicFor.Create(grilla);
  UtilGrilla.IniEncab;
  UtilGrilla.AgrEncab('' , 10).visible := false;  //para gaurdar cambios
  UtilGrilla.AgrEncab('Address' , 30);  //Con 40 pixeles de ancho
  UtilGrilla.AgrEncab('Name' , 40);  //Con 60 pixeles de ancho
  UtilGrilla.AgrEncab('Value' , 40, -1, taRightJustify); //Justificado a la derecha
  UtilGrilla.FinEncab;
  UtilGrilla.OnFinEditarCelda := @UtilGrillaFinEditarCelda;
  Utilgrilla.OnLeerColorFondo := @UtilGrillaLeerColorFondo;
  Utilgrilla.OnKeyDown := @UtilGrillaKeyDown;
  grilla.OnKeyUp := @grillaKeyUp;

  grilla.RowCount := 2;
//  grilla.FixedCols := 0;
  grilla.Options := grilla.Options + [goColSizing];
end;
destructor TfraRegWatcher.Destroy;
begin
  UtilGrilla.Destroy;
  inherited Destroy;
end;

end.

