{Formulario para mostrar las propiedades de un elemento.}
unit FormElemProperty;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls, XpresElementsPIC;
type

  { TfrmElemProperty }

  TfrmElemProperty = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Image1: TImage;
    ImageList1: TImageList;
    lblElemName4: TLabel;
    lblElemName5: TLabel;
    lblUsed: TLabel;
    lblElemName1: TLabel;
    lblElemName2: TLabel;
    lblElemName3: TLabel;
    txtEleLocFile: TEdit;
    txtEleName: TEdit;
    lblElemName: TLabel;
    Memo1: TMemo;
    txtEleType: TEdit;
    txtEleLocaPath: TEdit;
    procedure BitBtn2Click(Sender: TObject);
  private
    elem: TxpElement;
  public
    OnExplore: procedure(elem0: TxpElement) of object;
    procedure Clear;
    procedure Exec(elem0: TxpElement);
  end;

var
  frmElemProperty: TfrmElemProperty;

implementation
{$R *.lfm}
{ TfrmElemProperty }
procedure TfrmElemProperty.BitBtn2Click(Sender: TObject);
begin
  if OnExplore<>nil then OnExplore(elem);
end;
procedure TfrmElemProperty.Clear;
begin
  txtEleName.Caption := 'Unknown';
  txtEleType.Caption := 'Unknown';
  txtEleLocaPath.Caption := '';
  txtEleLocFile.Caption := '';
  lblUsed.Font.Color := clGray;
  lblUsed.Caption := 'Unused';
  ImageList1.GetBitmap(13, Image1.Picture.Bitmap);
  Memo1.Text := '';
  BitBtn2.Enabled := false;
end;
procedure TfrmElemProperty.Exec(elem0: TxpElement);
var
  adicInformation: String;
  fun: TxpEleFun;
  xvar: TxpEleVar;
begin
  if elem0 = nil then exit;
  elem := elem0;
  Image1.Stretch := true;
  Image1.Proportional := true;  // to keep width/height ratio
  adicInformation := '';
  txtEleName.Caption := elem.name;
  if elem.typ = nil then begin
    txtEleType.Caption := 'Unknown';
  end else begin
    txtEleType.Caption := elem.typ.name;
  end;
  txtEleLocaPath.Caption := ExtractFileDir(elem.srcDec.Fil);
  txtEleLocFile.Caption := ExtractFileName(elem.srcDec.Fil) + ' [' + IntToStr(elem.srcDec.Row) + ',' +
                                             IntToStr(elem.srcDec.Col)+']';
  BitBtn2.Enabled := true;
  if elem.nCalled = 0 then begin
      lblUsed.Font.Color := clGray;
      lblUsed.Caption := 'Unused';
  end else begin
      lblUsed.Font.Color := clGreen;
      lblUsed.Caption := 'Used ' + IntToStr(elem.nCalled) + ' times.';
  end;
  if elem is TxpEleCon then begin
    ImageList1.GetBitmap(4, Image1.Picture.Bitmap);
    adicInformation := '';
  end else if elem is TxpEleVar then begin
    ImageList1.GetBitmap(2, Image1.Picture.Bitmap);

    xvar := TxpEleVar(elem);
    adicInformation := 'Direcc. Solicitada: ' + IntToStr(xvar.solAdr) + ':' + IntToStr(xvar.solBit) + LineEnding +
           'Direcc. Asignada: ' + xvar.AddrString;
  end else if elem is TxpEleFun then begin
    ImageList1.GetBitmap(3, Image1.Picture.Bitmap);
    fun := TxpEleFun(elem);
    adicInformation := 'Dirección: $' + IntToHex(fun.adrr, 3) + LineEnding +
           'Tamaño: ' + IntToStr(fun.srcSize);
  end else if elem is TxpEleUnit then begin
    ImageList1.GetBitmap(6, Image1.Picture.Bitmap);
    adicInformation := '';
  end else if elem is TxpEleBody then begin
    ImageList1.GetBitmap(12, Image1.Picture.Bitmap);
    adicInformation := 'Dirección: $' + IntToHex(fun.adrr, 3) + LineEnding +
           'Tamaño: ' + IntToStr(fun.srcSize);
  end else begin
    ImageList1.GetBitmap(13, Image1.Picture.Bitmap);
    adicInformation := '';
  end;
  Memo1.Text := adicInformation;
end;

end.


