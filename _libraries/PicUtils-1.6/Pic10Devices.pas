{Unit containing the devices supported by the directive $PROCESSOR.
The list of devices supported doesn't have to be complete, because the use
of directive $PROCESSOR is no recommended. It's preferred using units with
device definition like PIC10F200.pas.
}
unit Pic10Devices;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils , PicCore, Pic10Utils;

procedure GetSupportedDevices(list: TStrings);
function GetHardwareInfo(pic: TPIC10; model: string): boolean;

implementation

procedure GetSupportedDevices(list: TStrings);
{Devuelve la lista de dispositivos soportados. Esta lista debe ser la misma que hay en
 GetHardwareInfo. }
begin
   list.Add('PIC10F200');
   list.Add('PIC10F202');
   list.Add('PIC10F204');
   list.Add('PIC10F206');
end;
function GetHardwareInfo(pic: TPIC10; model: string): boolean;
{Obtiene información parra un modelo de PIC en especial. Si no lo encuentra, devuelve
 FALSE}
begin
   Result := true;
   pic.MaxFlash := 0;  //inicia valor
   pic.MaxFreq := 0;
   pic.DisableAllRAM;
   pic.Model := model;
   case Upcase(model) of
   'DEFAULT': begin   //Configuración por defecto. Se toma la del PIC16F84
     pic.MaxFreq:=10000000;  //Pero a 20MHz
     pic.NumBanks:=1;
     pic.NumPages:=1; pic.MaxFlash:=256;  //banco 0 implementado parcialmente
     pic.SetStatRAMCom('000-007:SFR, 010-01F:GPR');
     //Hardware definition
     pic.Npins := 6;
     pic.ram[$006].name := 'GPIO';   //Pone un nombre, para que MapRAMtoPIN, asigne nombre a los pines
     pic.MapRAMtoPIN('006:0-1,1-3,2-4,3-6');
     PIC.SetPin(2, 'VSS', pptGND);
     PIC.SetPin(5, 'VDD', pptVcc);
     end;
   'PIC10F200' : begin
     pic.MaxFreq:=4000000;
     pic.Npins := 6;
     pic.NumBanks:=1;
     pic.NumPages:=1; pic.MaxFlash:=256;  //banco 0 implementado parcialmente
     pic.SetStatRAMCom('000-007:SFR, 010-01F:GPR');
     //Hardware definition
     pic.Npins := 6;
     pic.ram[$006].name := 'GPIO';   //Pone un nombre, para que MapRAMtoPIN, asigne nombre a los pines
     pic.MapRAMtoPIN('006:0-1,1-3,2-4,3-6');
     PIC.SetPin(2, 'VSS', pptGND);
     PIC.SetPin(5, 'VDD', pptVcc);
   end;
   'PIC10F202' : begin
     pic.MaxFreq:=4000000;
     pic.Npins := 6;
     pic.NumBanks:=1;
     pic.NumPages:=1; pic.MaxFlash:=512;  //banco 0 implementado parcialmente
     pic.SetStatRAMCom('000-007:SFR, 008-01F:GPR');
     //Hardware definition
     pic.Npins := 6;
     pic.ram[$006].name := 'GPIO';   //Pone un nombre, para que MapRAMtoPIN, asigne nombre a los pines
     pic.MapRAMtoPIN('006:0-1,1-3,2-4,3-6');
     PIC.SetPin(2, 'VSS', pptGND);
     PIC.SetPin(5, 'VDD', pptVcc);
   end;
   'PIC10F204' : begin
     pic.MaxFreq:=4000000;
     pic.Npins := 6;
     pic.NumBanks:=1;
     pic.NumPages:=1; pic.MaxFlash:=256;  //banco 0 implementado parcialmente
     pic.SetStatRAMCom('000-007:SFR, 010-01F:GPR');
     //Hardware definition
     pic.Npins := 6;
     pic.ram[$006].name := 'GPIO';   //Pone un nombre, para que MapRAMtoPIN, asigne nombre a los pines
     pic.MapRAMtoPIN('006:0-1,1-3,2-4,3-6');
     PIC.SetPin(2, 'VSS', pptGND);
     PIC.SetPin(5, 'VDD', pptVcc);
   end;
   'PIC10F206' : begin
     pic.MaxFreq:=4000000;
     pic.Npins := 6;
     pic.NumBanks:=1;
     pic.NumPages:=1; pic.MaxFlash:=512;  //banco 0 implementado parcialmente
     pic.SetStatRAMCom('000-007:SFR, 008-01F:GPR');
     //Hardware definition
     pic.Npins := 6;
     pic.ram[$006].name := 'GPIO';   //Pone un nombre, para que MapRAMtoPIN, asigne nombre a los pines
     pic.MapRAMtoPIN('006:0-1,1-3,2-4,3-6');
     PIC.SetPin(2, 'VSS', pptGND);
     PIC.SetPin(5, 'VDD', pptVcc);
   end;
   else
     exit(false);
   end;
end;

end.

