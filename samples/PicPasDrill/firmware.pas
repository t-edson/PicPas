program drillman;
// Drillmotor-Controller with PIC12F629
// Compiler: PicPas v0.72 (07.2017)
// Config-Word: $31F4

{$MODE PASCAL} 
{$FREQUENCY 8 MHZ }
{$PROCESSOR PIC12F629}

// Connections: 
// GP0 and GP1 : Comparator inputs (internal comparator)
// GP2 : Motor-Driver
// GP3 : RESET (MCLR-function)
// GP4 : failure indicator, motor blocked, red LED 
// GP5 : failure indicator, overload, red LED

var 
  GPIO    : byte absolute $05;
  TRISIO  : byte absolute $85; 
  CMCON   : byte absolute $19;
  MOTOR   : bit  absolute GPIO.2;  // motor-driver
  BLOCKED : bit  absolute GPIO.4;  // LED 
  OVRLOAD : bit  absolute GPIO.5;  // LED
  COMPOUT : bit  absolute CMCON.6; // internal Comparator Output

  dly     : byte; // used for delays...
  blk     : byte; // counts up the Motor-ON-states
  ovl     : byte; // counts up the power-supply overloads
  
const
  blklim  = 250;  // limit to detect motor is blocked
  ovllim  = 250;  // limit to detect power-supply overload
  
// Emergency-Stop  
procedure halt;
begin
  MOTOR := 0;
  repeat                       // Wait for RESET-press
  until false;
end;    
  
  
// Main  
begin 
 
  CMCON   := 2;                // CM_Mode_without_Output for comparator   
  TRISIO  := %11001011;        // three outputs: motor, blocked, ovrload
  GPIO    := %00000011;        // all outputs off
//  MOTOR   := 0;
//  BLOCKED := 0;
//  OVRLOAD := 0;
  blk := 0;
  ovl := 0;
  
  repeat                       // start the main-loop
    if (COMPOUT=1) then        // if motor has to be powered
    begin
      MOTOR := 1;              // motor Power-On
      inc(blk);
      dly := 0;
      repeat                   // constant pulse-width
        dec(dly);
      until (dly=0);         
      if (COMPOUT=1) then  inc(ovl) // check supply voltage  
                     else  ovl := 0;
      MOTOR := 0;              // motor Power-Off
      dly := 20;
      repeat                   // small delay, emf-voltage settle
        dec(dly);
      until (dly=0) OR (COMPOUT=1);      
    end else blk := 0;
    if (ovl>ovllim) then       // if power-supply overload
    begin
      OVRLOAD := 1;            // Error-LED "OVERLOAD" on
      halt;                    // FAILURE, Emergency STOP
    end; 
    if (blk>blklim) then       // if motor is blocked
    begin 
      BLOCKED := 1;            // Error-LED "BLOCKED" on
      halt;                    // FAILURE, Emergency STOP
    end; 
  until false;                 // End of main-loop
end.

