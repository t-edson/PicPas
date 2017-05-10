program aaa; //No es necesario ver duplicidad, es el primer identificador.
uses  Pic16f84a;
const 
  a = 1;        //Verificación de duplicidad.
	b = a + 1;
var
  x: byte; 
  y: word;   //Verificación de duplicidad.

procedure proc(par1: byte); //Verificación de duplicidad, excepto el primero.
var 
  x: byte;
begin
  //x := 1;
//  portB := 1;
end;

var
  x1: byte; //Verificación de duplicidad.

begin
   //Cuerpo del programa
  proc(1);
//  portB := 1;
//  trisA := $22;
end.

