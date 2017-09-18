{Description of the program.}
program nombre;
var x,y: byte;
procedure proc1;
begin
  x := 1;
end; 
procedure proc2;
begin
  x := 1;
end; 
begin
//Prueba de funciones
  {$MSGBOX abs(-1)}
  {$MSGBOX sgn(-1000)}
  {$MSGBOX sin(3.1415)}
  {$MSGBOX cos(0)}
  {$MSGBOX tan(0)}
  {$MSGBOX log(2.718)}
  {$MSGBOX round(3.5)}
  {$MSGBOX round(2.1)}
  {$MSGBOX trunc(2.9)}
  {$MSGBOX length('Hola mundo')}
  {$MSGBOX length('tú')}
  {$MSGBOX Upcase('Hola mundo')}

//Define características del hardware
{$SET PIC_MODEL='MIPIC'}
{$SET PIC_MAXFREQ = 1000000}
{$SET PIC_NPINS = 18}
{$SET PIC_NUMBANKS=2}
{$SET PIC_NUMPAGES = 1}
{$SET PIC_MAXFLASH = 1024}
//Inicia la memoria, para empezar a definir
{$SET_STATE_RAM '000-1FF:NIM'}
//Define estado de la memoria RAM
{$SET_STATE_RAM '000-00B:SFR, 00C-04F:GPR'}
{$SET_STATE_RAM '080-08B:SFR, 08C-0CF:GPR'}
//Define zonas de memoria mapeada
{$SET_MAPPED_RAM '080-080:bnk0, 082-084:bnk0, 08A-08B:bnk0'}
{$SET_MAPPED_RAM '08C-0CF:bnk0'}

end. 
