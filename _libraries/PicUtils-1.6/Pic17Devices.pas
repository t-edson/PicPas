{Unit containing the devices supported by the directive $PROCESSOR.
The list of devices supported doesn't have to be complete, because the use
of directive $PROCESSOR is no recommended. It's preferred using units with
device definition.
}
unit Pic17Devices;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils , PicCore, Pic17Utils;

procedure GetSupportedDevices(list: TStrings);
function GetHardwareInfo(pic: TPIC17; model: string): boolean;

implementation

procedure GetSupportedDevices(list: TStrings);
{Devuelve la lista de dispositivos soportados. Esta lista debe ser la misma que hay en
 GetHardwareInfo. }
begin
   //list.Add('PIC16F1503');
end;
function GetHardwareInfo(pic: TPIC17; model: string): boolean;
{Obtiene información parra un modelo de PIC en especial. Si no lo encuentra, devuelve
 FALSE}
begin
   Result := true;
   pic.MaxFlash := 0;  //inicia valor
   pic.MaxFreq := 0;
   pic.DisableAllRAM;
   pic.Model := model;
   case Upcase(model) of
   'DEFAULT': begin   //Defaul configuration. Similar to (but no equal) PIC16F1503
     pic.MaxFreq:=20000000;
     pic.Npins := 14;
     pic.NumBanks:=32;  //Really only some banks are used by SFR and GPR are in bank 0 and 1
     pic.NumPages:=1; pic.MaxFlash:=2048;  //All bank 0
     pic.SetStatRAMCom('000-01F:SFR, 020-07F:GPR');
     pic.SetStatRAMCom('080-09F:SFR, 0A0-0BF:GPR, 0F0-0FF:GPR');
     pic.SetStatRAMCom('100-11F:SFR, 170-17F:GPR');
     pic.SetStatRAMCom('180-19F:SFR, 1F0-1FF:GPR');
     pic.SetStatRAMCom('200-21F:SFR, 270-27F:GPR');
     pic.SetStatRAMCom('280-29F:SFR, 2F0-2FF:GPR');
     pic.SetStatRAMCom('300-31F:SFR, 370-37F:GPR');
     pic.SetStatRAMCom('380-39F:SFR, 3F0-3FF:GPR');
     pic.SetStatRAMCom('400-41F:SFR, 470-47F:GPR');
     pic.SetStatRAMCom('480-49F:SFR, 4F0-4FF:GPR');
     pic.SetStatRAMCom('500-51F:SFR, 570-57F:GPR');
     pic.SetStatRAMCom('580-59F:SFR, 5F0-5FF:GPR');
     pic.SetStatRAMCom('600-61F:SFR, 670-67F:GPR');
     pic.SetStatRAMCom('680-69F:SFR, 6F0-6FF:GPR');
     pic.SetStatRAMCom('700-71F:SFR, 770-77F:GPR');
     pic.SetStatRAMCom('780-79F:SFR, 7F0-7FF:GPR');
     pic.SetStatRAMCom('800-81F:SFR, 870-87F:GPR');
     pic.SetStatRAMCom('880-89F:SFR, 8F0-8FF:GPR');
     pic.SetStatRAMCom('900-91F:SFR, 970-97F:GPR');
     pic.SetStatRAMCom('980-99F:SFR, 9F0-9FF:GPR');
     pic.SetStatRAMCom('A00-A1F:SFR, A70-A7F:GPR');
     pic.SetStatRAMCom('A80-A9F:SFR, AF0-AFF:GPR');
     pic.SetStatRAMCom('B00-B1F:SFR, B70-B7F:GPR');
     pic.SetStatRAMCom('B80-B9F:SFR, BF0-BFF:GPR');
     pic.SetStatRAMCom('C00-C1F:SFR, C70-C7F:GPR');
     pic.SetStatRAMCom('C80-C9F:SFR, CF0-CFF:GPR');
     pic.SetStatRAMCom('D00-D1F:SFR, D70-D7F:GPR');
     pic.SetStatRAMCom('D80-D9F:SFR, DF0-DFF:GPR');
     pic.SetStatRAMCom('E00-E1F:SFR, E70-E7F:GPR');
     pic.SetStatRAMCom('E80-E9F:SFR, EF0-EFF:GPR');
     pic.SetStatRAMCom('F00-F6F:SFR, F70-F7F:GPR');  //More SFR
     pic.SetStatRAMCom('F80-FEF:SFR, FF0-FFF:GPR');  //More SFR
     //Hardware definition
     pic.Npins := 14;
     pic.ram[$00C].name := 'PORTA';   //Pone un nombre, para que MapRAMtoPIN, asigne nombre a los pines
     pic.ram[$00E].name := 'PORTC';   //Pone un nombre, para que MapRAMtoPIN, asigne nombre a los pines
     pic.MapRAMtoPIN('00C:0-13,1-12,2-11,3-4,4-3,5-2');
     pic.MapRAMtoPIN('00E:0-10,1-9,2-8,3-7,4-6,5-5');
     PIC.SetPin(14, 'VSS', pptGND);
     PIC.SetPin(1, 'VDD', pptVcc);
   end;
   else
     exit(false);
   end;
end;

end.

