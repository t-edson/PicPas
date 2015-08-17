{Ejemplo de Frame de propiedades }
unit frameTexto;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  ConfigFrame;  //necesario para manejar los Frames de configuración

type

  { TfraTexto }

  TfraTexto = class(TCfgFrame)
    Edit1: TEdit;
  public
    //variables de propiedades
    texto : string;
    procedure Iniciar(secINI0: string); //Inicia el frame
  end;

implementation
{$R *.lfm}

{ TfraTexto }

procedure TfraTexto.Iniciar(secINI0: string);
begin
  secINI := secINI0;  //sección INI
  //asocia propiedades a controles
  Asoc_Str_TEdit(@texto, Edit1, 'texto', '');
end;

end.

