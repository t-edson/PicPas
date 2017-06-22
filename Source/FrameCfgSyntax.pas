unit FrameCfgSyntax;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazUTF8, Forms, Controls, StdCtrls,
  LCLProc, Graphics, MisUtils, fgl, LCLIntf, Dialogs, SynFacilBasic, strutils;

type
  //Registro para modelar la posición de un parámetro de un Nodo CML

  { TParamPos }

  TParamPos = object
    lines: TStringList;   //Referncia a archivo de sintaxis
    nlin : integer;       //Índice de línea
    p1, p2: integer;      //Posición de inicio y fin del parámetro
    function Exist: boolean;
    function ReadString: string;
    function ReadColor: TColor;
    function ReadBool: boolean;
    procedure WriteString(value: string);
    procedure WriteColor(color: TColor);
    procedure WriteBool(value: boolean);
  end;

  { TSynAttribute }

  TSynAttribute = class
  private
    function GetName: string;
    procedure SetName(AValue: string);
  public
    pName: TParamPos;
    pTextColor: TParamPos;
    pBackColor: TParamPos;
    pBold  : TParamPos;
    pItalic: TParamPos;
    pUnder : TParamPos;
  end;
  TSynAttributeList = specialize TFPGObjectList<TSynAttribute>;

  { TSynLang }
  {Almacena al archivo de sintaxis, y alguna posiciones importantes para poder
   modificarlo. No usa documentos XML, sino que lo maneja el archivo como líneas de
  texto para manteenr el formato, en las líneas no editadas.}
  TSynLang = class
  private
    lines : TStringList;  //Contenedor del archivo de sintaxis
    linComplet: integer;  //línea donde esta <Completion>
    linLangua : integer;  //línea donde esta <Language>
    pName: TParamPos;
    pOpenOnKeyUp: TParamPos;
  public
    filName: string;
    Attributes: TSynAttributeList;  //Lista de atributos
  public
    procedure ReadFromFile(fil: string);
    procedure SaveToFile;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TSynLangList = specialize TFPGObjectList<TSynLang>;

  { TfraCfgSyntax }
  TfraCfgSyntax = class(TFrame)
    chkBold: TCheckBox;
    chkAutoComp: TCheckBox;
    chkItalic: TCheckBox;
    chkUnder: TCheckBox;
    colTextCol: TColorButton;
    colBackCol: TColorButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListBox1: TListBox;
    procedure chkAutoCompChange(Sender: TObject);
    procedure colTextColColorChanged(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    pathSyn: string;
    curLang: TSynLang;   //Sintaxis actual
    curAttr: TSynAttribute;  //Atributo actual
    synLangList: TSynLangList;
    function AddSyntax(synFile: string): TSynLang;
  public
    procedure Init(pathSyn0: string);
    procedure SaveChanges;
  public
    //genera constructor y destructor
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
function ReadParamPos(lines: TStringList; nlin: integer; parName: string): TParamPos;
{Devuelve un TParamPos, con la posición, en la cadena, del parámetro indicado.
 Si no enecuentra, devuelve TParamPos.p1:=0}
var
  p1: SizeInt;
  carStr: Char;
  lin: String;
begin
  Result.lines := lines;
  Result.nlin := nlin;
  lin := lines[nlin];
  //Busca el inicio del parámetro
  lin := UpCase(lin);  //Para realizar la búsqueda sin considera caja
  parName := UpCase(parName + '=');
  p1 := pos(parName, lin);
  if p1=0 then begin
    //No enceontró
    Result.p1 := 0;
    exit;
  end else begin
    //Encontró la cadena
    p1 := p1 + length(parName);  //Para que pase
    while (p1<=length(lin)) and (lin[p1] in [' ',#9]) do begin
      inc(p1);
    end;
    //No debería fallar, si ya se cargó la sintaxis
    if p1>length(lin) then begin
      Result.p1 := 0;
      exit;
    end;
    //Debería seguir comilla o doble comilla
    carStr := lin[p1];
    if not (carStr in ['''', '"']) then begin
      Result.p1 := 0;
      exit;
    end;
    //Busca el final de cadena
    Result.p1 := p1 +1;
    Result.p2 := posEx(carStr, lin, p1+1)-1;
  end;
end;

{ TParamPos }
function TParamPos.Exist: boolean;
begin
  Result := p1<>0;
end;
function TParamPos.ReadString: string;
begin
  if p1 = 0 then exit('');
  Result := copy(lines[nlin], p1, p2 - p1 + 1);
end;
function TParamPos.ReadColor: TColor;
  function EsHexa(txt: string; out num: integer): boolean;
  //Convierte un texto en un número entero. Si es numérico devuelve TRUE
  var
    i: integer;
  begin
    Result := true;  //valor por defecto
    num := 0; //valor por defecto
    for i:=1 to length(txt) do begin
      if not (txt[i] in ['0'..'9','a'..'f','A'..'F']) then exit(false);  //no era
    end;
    //todos los dígitos son numéricos
    num := StrToInt('$'+txt);
  end;
var
  cad: String;
begin
  if p1 = 0 then exit(clBlack);
  cad := ReadString;
  Result := clBlack;  //Color por defecto
  Result := ColorFromStr(cad);
end;
function TParamPos.ReadBool: boolean;
begin
  if p1=0 then exit(false);
  Result := UpCase(ReadString)='TRUE';
end;
procedure TParamPos.WriteString(value: string);
var
  lin: String;
begin
  if p1 = 0 then exit;
  lin := lines[nlin];
  lines[nlin] := copy(lin, 1, p1-1)+ value + copy(lin, p2+1, length(lin));
end;
procedure TParamPos.WriteColor(color: TColor);
var
  value: String;
  r, g, b: Integer;
begin
  if p1 = 0 then exit;
  r := color and $FF;
  g := (color >> 8) and $FF;
  b := (color >> 16) and $FF;
  value := '#' + IntToHex(r,2) + IntToHex(g,2) + IntToHex(b,2);
  WriteString(value);
end;
procedure TParamPos.WriteBool(value: boolean);
begin
  if p1=0 then exit;
  if Value then WriteString('True') else WriteString('False');
end;
{ TSynAttribute }
function TSynAttribute.GetName: string;
begin
  Result := pName.ReadString;
end;
procedure TSynAttribute.SetName(AValue: string);
begin
  pName.WriteString(AValue);
end;
{ TSynLang }
procedure TSynLang.ReadFromFile(fil: string);
var
  lin: String;
  i: Integer;
  att: TSynAttribute;
begin
  filName := fil;
  lines.LoadFromFile(fil);
  linComplet := 0;
  for i := 0 to lines.Count-1 do begin
    lin := lines[i];
    if AnsiContainsText(lin, '<Completion') then begin
      linComplet := i;
      pOpenOnKeyUp := ReadParamPos(lines, i, 'OpenOnKeyUp');
    end else if AnsiContainsText(lin, '<Language') then begin
      linLangua := i;
      pName := ReadParamPos(lines, i, 'Name');
    end else if AnsiContainsText(lin, '<Attribute') then begin
      //Crea el atributo
      att := TSynAttribute.Create;
      att.pName := ReadParamPos(lines, i, 'Name');
      att.pTextColor := ReadParamPos(lines, i, 'ForeCol');
      att.pBackColor := ReadParamPos(lines, i, 'BackCol');
      att.pBold      := ReadParamPos(lines, i, 'Bold');
      att.pItalic    := ReadParamPos(lines, i, 'Italic');
      att.pUnder     := ReadParamPos(lines, i, 'Underline');
      Attributes.Add(att);
    end;
  end;

end;
procedure TSynLang.SaveToFile;
begin
  lines.SaveToFile(filName);
end;
constructor TSynLang.Create;
begin
  lines := TStringList.Create;
  Attributes:= TSynAttributeList.Create(true);
end;
destructor TSynLang.Destroy;
begin
  lines.Destroy;
  Attributes.Destroy;
  inherited Destroy;
end;
procedure TfraCfgSyntax.chkAutoCompChange(Sender: TObject);
begin
  if curLang = nil then exit;
  curLang.pOpenOnKeyUp.WriteBool(chkAutoComp.Checked);
end;
procedure TfraCfgSyntax.colTextColColorChanged(Sender: TObject);
begin
  if curAttr = nil then exit;
  curAttr.pTextColor.WriteColor(colTextCol.ButtonColor);
end;

procedure TfraCfgSyntax.ComboBox1Change(Sender: TObject);
var
  att: TSynAttribute;
begin
  if ComboBox1.ItemIndex = -1 then begin
    curLang := nil;
    exit;
  end;
  //Actualiza curSynLang
  curLang := synLangList[ComboBox1.ItemIndex];
  //Llena las propiedades
  ListBox1.Clear;
  if curLang.linComplet= 0 then begin
    //No tiene inforrmación de completado
    chkAutoComp.Enabled := false;
  end else begin //Sí tiene completado
    chkAutoComp.Enabled := true;
    chkAutoComp.Checked := curLang.pOpenOnKeyUp.ReadBool;
  end;
  //Llena los atributos encontrados
  for att in curLang.Attributes do begin
    ListBox1.AddItem(att.pName.ReadString, att);  //Guarda referencia al objeto
  end;
  if ListBox1.Count>0 then begin
    ListBox1.ItemIndex := 0;
    ListBox1Click(self);   //Actualiza
  end;
end;
procedure TfraCfgSyntax.ListBox1Click(Sender: TObject);
{Se selecciona un atributo de la lista de atributos.}
var
  param: TParamPos;
  att: TSynAttribute;
begin
  if ListBox1.ItemIndex = -1 then begin
    curAttr := nil;
    exit;
  end;
  curAttr := nil;  //Se poene en NIl, para evitar disparar eventos en los botones
  att := TSynAttribute(ListBox1.Items.Objects[ListBox1.ItemIndex]);

  //  MsgBox(att.pName.ReadString);
  param := att.pTextColor;
  colTextCol.Enabled := param.Exist;
  Label2.Enabled := param.Exist;
  if param.Exist then colTextCol.ButtonColor := param.ReadColor;

  param := att.pBackColor;
  colBackCol.Enabled := param.Exist;
  Label4.Enabled := param.Exist;
  if param.Exist then  colBackCol.ButtonColor := param.ReadColor;

  chkBold.Enabled := att.pBold.Exist;
  if att.pBold.Exist then chkBold.Checked := att.pBold.ReadBool;

  chkItalic.Enabled := att.pItalic.Exist;
  if att.pItalic.Exist then chkItalic.Checked := att.pItalic.ReadBool;

  chkUnder.Enabled := att.pUnder.Exist;
  if att.pUnder.Exist then chkUnder.Checked := att.pUnder.ReadBool;
  //Actualiza al final "curAttr".
  curAttr := att;
end;
function TfraCfgSyntax.AddSyntax(synFile: string): TSynLang;
{Agrega una sintaxis a la lista de sintaxis. Devuelve la referecnia a la sinatxis}
var
  synLan: TSynLang;
begin
  try
    //Agrega sintaxis a la lista
    synLan := TSynLang.Create;
    synLan.ReadFromFile(synFile);
    synLangList.add(synLan);
    Result := synLan;
  except
    Result := nil;
  end;
end;
{ TfraCfgSyntax }
procedure TfraCfgSyntax.Init(pathSyn0: string);
var
  directorio, nomArc: String;
  SearchRec: TSearchRec;
  synt: TSynLang;
begin
  pathSyn := pathSyn0;
  ComboBox1.Clear;
  directorio := pathSyn;
  if FindFirst(directorio + DirectorySeparator + '*.xml', faDirectory, SearchRec) = 0 then begin
    repeat
      nomArc := SysToUTF8(SearchRec.Name);
      if SearchRec.Attr and faDirectory = faDirectory then begin
        //directorio
      end else begin //archivo
        //Agrega la sintaxis
        synt := AddSyntax(directorio + DirectorySeparator + nomArc);
        //Argega nombre de archivo, sin extensión
        nomArc := copy(nomArc, 1, length(nomArc)-4);  //quita extensión
        delete(nomArc,1, 6);  //quita parte inicial
        ComboBox1.AddItem(synt.pName.ReadString, nil);
      end;
    until FindNext(SearchRec) <> 0;
    //Ya no hay más archivos
    FindClose(SearchRec);
  end;
  //Actualiza
  if ComboBox1.Items.Count = 0 then exit;
  ComboBox1.ItemIndex := 0;
  ComboBox1Change(self);
end;
procedure TfraCfgSyntax.SaveChanges;
var
  synLang: TSynLang;
begin
  for synLang in synLangList do begin
    synLang.SaveToFile;
  end;
end;
constructor TfraCfgSyntax.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  synLangList:= TSynLangList.Create(true);
end;
destructor TfraCfgSyntax.Destroy;
begin
  synLangList.Destroy;
  inherited Destroy;
end;

end.

