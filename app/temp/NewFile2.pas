////////////////////////////////////////////
// New program created in 12/07/2019}
////////////////////////////////////////////
program NewProgram;
uses PIC10F200;
{$FREQUENCY 4MHZ}
begin
  SetAsOutput(GPIO_GP0);
  //Code here
  while true do
    delay_ms(100);
  end; 
end.
