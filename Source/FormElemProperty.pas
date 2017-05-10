{Formulario para mostrar las propiedades de un elemento.}
unit FormElemProperty;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;
type

  { TfrmElemProperty }

  TfrmElemProperty = class(TForm)
    BitBtn1: TBitBtn;
    Memo1: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmElemProperty: TfrmElemProperty;

implementation

{$R *.lfm}

end.

