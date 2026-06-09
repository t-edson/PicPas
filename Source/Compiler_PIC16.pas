{Unidad con rutinas del analizador sintáctico.
}
unit Compiler_PIC16;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, lclProc, SynEditHighlighter, MisUtils, XpresBas,
  XpresTypesPIC, XpresElementsPIC, Pic16Utils, Pic16Devices, CompBase,
  ParserDirec, GenCodBas_PIC16, GenCod_PIC16, ParserDirec_PIC16, Globales,
  FormConfig, CompOperands {Por diseño, parecería que GenCodBas, no debería accederse desde aquí};
type
  { TCompiler_PIC16 }
  TCompiler_PIC16 = class(TParserDirec)
  private   //Funciones básicas
    procedure cInNewLine(lin: string);
  private //Compilación de secciones
    procedure CompileLinkProgram;
  public
    procedure Compile(NombArc: string; Link: boolean); override;
    procedure RAMusage(lins: TStrings; varDecType: TVarDecType; ExcUnused: boolean); override; //uso de memoria RAM
    procedure DumpCode(lins: TSTrings; incAdrr, incCom, incVarNam: boolean); override; //uso de la memoria Flash
    function RAMusedStr: string; override;
    function FLASHusedStr: string; override;
    procedure GetResourcesUsed(out ramUse, romUse, stkUse: single); override;
    procedure GenerateListReport(lins: TStrings); override;
  public //Inicialización
    constructor Create(msg0: TMessageManager);
    destructor Destroy; override;
  end;

procedure SetLanguage;

implementation
var
  ER_NOT_IMPLEM_, ER_IDEN_EXPECT, ER_DUPLIC_IDEN, ER_INVAL_FLOAT: string;
  ER_NOTYPDEFNUM, ER_EXP_VAR_IDE, ER_INV_MEMADDR, ER_BIT_VAR_REF: String;
  ER_UNKNOWN_ID_: string;
  ER_IDE_CON_EXP, ER_NUM_ADD_EXP, ER_IDE_TYP_EXP, ER_SEM_COM_EXP: String;
  ER_EQU_COM_EXP, ER_PROG_NAM_EX, ER_COMPIL_PROC, ER_CON_EXP_EXP: String;
  ER_FIL_NOFOUND, WA_UNUSED_CON_, WA_UNUSED_VAR_,WA_UNUSED_PRO_: String;
  MSG_RAM_USED, MSG_FLS_USED, ER_ARR_SIZ_BIG: String;
  ER_INV_ARR_SIZ: String;
//Funciones básicas
procedure SetLanguage;
begin
  ParserDirec_PIC16.SetLanguage;
  {$I ..\language\tra_Compiler.pas}
end;
procedure TCompiler_PIC16.cInNewLine(lin: string);
//Se pasa a una nueva _Línea en el contexto de entrada
begin
  if Config.IncComment then begin
    pic.addTopComm('    ;'+trim(lin));  //agrega _Línea al código ensmblador
  end;
end;

procedure TCompiler_PIC16.CompileLinkProgram;
{Genera el código compilado final. Usa la información del árbol de sintaxis, para
ubicar a los diversos elementos que deben compilarse.
Se debe llamar después de compilar con CompileProgram.
Esto es lo más cercano a un enlazador, que hay en PicPas.}
  procedure SetInitialBank(fun: TxpEleFun);
  {Define el banco de trabajo para compilar correctamente}
  var
    cal : TxpEleCaller;
  begin
    if fun.IsInterrupt then begin
      //Para ISR, no se genera código de manejo de bancos
      fun.iniBnk := 0;         //asume siempre 0
      CurrBank := fun.iniBnk;  //configura al compilador
      exit;
    end;
    if SetProIniBnk then begin
      _BANKRESET; //Se debe forzar a iniciar en el banco O
      fun.iniBnk := 0;   //graba
    end else begin
      //Se debe deducir el banco inicial de la función
      //Explora los bancos desde donde se llama
      if fun.lstCallers.Count = 1 then begin
        //Solo es llamado una vez
        fun.iniBnk := fun.lstCallers[0].curBnk;
        CurrBank := fun.iniBnk;  //configura al compilador
      end else begin
        fun.iniBnk := fun.lstCallers[0].curBnk;  //banco de la primera llamada
        //Hay varias llamadas
        for cal in fun.lstCallers do begin
          if fun.iniBnk <> cal.curBnk then begin
            //Hay llamadas desde varios bancos.
            _BANKRESET; //Se debe forzar a iniciar en el banco O
            fun.iniBnk := 0;   //graba
            exit;
          end;
        end;
        //Todas las llamadas son del mismo banco
        CurrBank := fun.iniBnk;  //configura al compilador
      end;
    end;
  end;
var
  elem   : TxpElement;
  bod    : TxpEleBody;
  xvar   : TxpEleVar;
  fun    : TxpEleFun;
  uni    : TxpEleUnit;
  iniMain, i: integer;
  add: Word;
begin
  ExprLevel := 0;
  pic.ClearMemFlash;
  callResetFlashAndRAM;
  ClearError;
  pic.MsjError := '';
  //Verifica las constantes usadas. Solo en el nodo principal, para no sobrecargar mensajes.
  for elem in TreeElems.main.elements do if elem.idClass = eltCons then begin
    if elem.nCalled = 0 then begin
      GenWarnPos(WA_UNUSED_CON_, [elem.name], elem.srcDec);
    end;
  end;
  pic.iFlash:= 0;  //inicia puntero a Flash
  RefreshAllElementLists;
  RemoveUnusedFunc;  //Se debe empezar con las funciones
  RemoveUnusedVars;  //Luego las variables
  RemoveUnusedCons;
  RemoveUnusedTypes;
  UpdateFunLstCalled;   //Actualiza lista "lstCalled" de las funciones usadas
  if HayError then exit;
  SeparateUsedFunctions;
  //Asigna memoria, primero a las variables locales (y parámetros) de las funciones
  CreateLocalVarsAndPars;
  ///////////////////////////////////////////////////////////////////////////////
  //Reserva espacio para las variables (Que no son de funciones).
  for xvar in TreeElems.AllVars do begin
    if xvar.Parent.idClass = eltFunc then continue;  //Las variables de funciones ya se crearon
    if xvar.Parent.idClass = eltUnit then continue;
    if xvar.nCalled>0 then begin
      //Asigna una dirección válida para esta variable
      AssignRAMtoVar(xvar);
      if HayError then exit;
    end else begin
      //Variable no usada
      xvar.ResetAddress;
      if xvar.Parent = TreeElems.main then begin
        //Genera mensaje solo para variables del programa principal.
        GenWarnPos(WA_UNUSED_VAR_, [xVar.name], xvar.srcDec);
      end;
    end;
  end;
  pic.iFlash:= 0;  //inicia puntero a Flash
  _GOTO_PEND(iniMain);       //instrucción de salto inicial
  //Codifica la función INTERRUPT, si existe
  if interruptFunct<>nil then begin;
    fun := interruptFunct;
    //Compila la función en la dirección 0x04
    pic.iFlash := $04;
    fun.adrr := pic.iFlash;    //Actualiza la dirección final
    fun.typ.DefineRegister;    //Asegura que se dispondrá de los RT necesarios
    lex.SetCtxState(fun.posCtx);  //Posiciona escáner
    PutLabel('__'+fun.name);
    TreeElems.OpenElement(fun.BodyNode); //Ubica el espacio de nombres, de forma similar a la pre-compilación
    SetInitialBank(fun);   //Configura manejo de bancos RAM
    callCompileProcBody(fun);
    TreeElems.CloseElement;  //cierra el body
    TreeElems.CloseElement;  //cierra la función
    if HayError then exit;     //Puede haber error
  end;
  //Codifica las subrutinas usadas
  for fun in usedFuncs do begin
    if fun.IsInterrupt then continue;
//debugln('---Función usada: ' + fun.name);
    {According the method we use to add callers (See TCompilerBase.AddCallerTo() ),
    condition "fun.nCalled>0" ensure we have here the first ocurrence of a function. So
    it can be:
      - A common function/procedure in the main program.
      - A INTERFACE function.
      - A private IMPLEMENTATION function (without previous declaration).
    }
    //Compile used function in the current address.
    fun.adrr := pic.iFlash;     //Actualiza la dirección final
    fun.typ.DefineRegister;   //Asegura que se dispondrá de los RT necesarios
    PutLabel('__'+fun.name);
    lex.SetCtxState(fun.posCtx); //Posiciona escáner
    if fun.compile<>nil then begin
      //Is system function. Has not body.
      fun.compile(fun);   //codifica
    end else begin
      //Is a common function with body.
      TreeElems.OpenElement(fun.BodyNode); //Ubica el espacio de nombres, de forma similar a la pre-compilación
      callCompileProcBody(fun);
      if HayError then exit;     //Puede haber error
      if pic.MsjError<>'' then begin //Error en el mismo PIC
          GenError(pic.MsjError);
          exit;
      end;
      TreeElems.CloseElement;  //cierra el body
      TreeElems.CloseElement;  //cierra la función
    end;
    fun.linked := true;      //Marca como ya enlazada
    //Verifica si hace falta completar llamadas
    if fun.nAddresPend>0 then begin
        //Hay llamadas pendientes qie completar a esta función
        for i:=0 to fun.nAddresPend -1 do begin
          debugln('Completando lllamadas pendientes a %s en %d', [fun.name, fun.addrsPend[i]]);
          //Completa la instrucción JSR $0000
          add := fun.addrsPend[i];
          pic.ram[add].value   := fun.adrr and $ff;
          pic.ram[add+1].value := (fun.adrr >> 8) and $ff;
        end;
      end;
  end;
  for fun in unusedFuncs do begin
    //Esta función no se usa.
    if fun.Parent = TreeElems.main then begin
      //Genera mensaje solo para funciones del programa principal.
      GenWarnPos(WA_UNUSED_PRO_, [fun.name], fun.srcDec);
    end;
  end;
  //Compila sección INITIALIZATION de las unidades usadas
  //Se debe seguir el orden de declaración
  for uni in TreeElems.AllUnits do begin
    debugln('>>'+uni.name);
  end;
  //Compila cuerpo del programa principal
  pic.codGotoAt(iniMain, _PC);   //termina de codificar el salto
  bod := TreeElems.BodyNode;  //lee Nodo del cuerpo principal
  if bod = nil then begin
    GenError('Body program not found.');
    exit;
  end;
  bod.adrr := pic.iFlash;  //guarda la dirección de codificación
//  bod.nCalled := 1;        //actualiza
  lex.SetCtxState(bod.posCtx);   //ubica escaner
  PutLabel('__main_program__');
  TreeElems.OpenElement(bod);
  CurrBank := 0;  //Se limpia, porque pudo haber cambiado con la compilación de procedimientos
  CompileCurBlock;
  TreeElems.CloseElement;   //cierra el cuerpo principal
  PutLabel('__end_program__');
  {No es necesario hacer más validaciones, porque ya se hicieron en la primera pasada}
  _SLEEP();   //agrega instrucción final
  if pic.MsjError<>'' then begin //Puede ser error al escribir la última instrucción
      GenError(pic.MsjError);
      exit;
  end;
end;
procedure TCompiler_PIC16.Compile(NombArc: string; Link: boolean);
//Compila el contenido de un archivo.
var
  p: SizeInt;
begin
  DefCompiler;   {Se crean los tipos de sistema, siempre, porque se pudo haber modificado
                  su definición, cuandos se definen tipos del usuario, como en el caso de
                  los punteros, que agregan operaciones a los word.}
  mode := modPicPas;   //Por defecto en sintaxis nueva
  mainFile := NombArc;
  hexfile := ChangeFileExt(NombArc, '.hex');     //Obtiene nombre
  hexfile := hexFilePath;   //Expande nombre si es necesario
  //se pone en un "try" para capturar errores y para tener un punto salida de salida
  //único
  if ejecProg then begin
    GenError(ER_COMPIL_PROC);
    exit;  //sale directamente
  end;
  try
    ejecProg := true;  //marca bandera
    ClearError;
    //Genera instrucciones de inicio
    lex.ClearAll;       //elimina todos los Contextos de entrada
    //Compila el texto indicado
    if not OpenContextFrom(NombArc) then begin
      //No lo encuentra
      GenError(ER_FIL_NOFOUND, [NombArc]);
      exit;
    end;
    {-------------------------------------------------}
    TreeElems.Clear;
    //Asigna nombre y archivo a elemento
    TreeElems.main.name := ExtractFileName(mainFile);
    p := pos('.',TreeElems.main.name);
    if p <> 0 then TreeElems.main.name := copy(TreeElems.main.name, 1, p-1);
    TreeElems.main.srcDec := lex.GetSrcPos;
    //Continúa con preparación
    TreeDirec.Clear;
    TreeElems.OnAddElement := @Tree_AddElement;   //Se va a modificar el árbol
    listFunSys.Clear;
    CreateSystemElements;  //Crea los elementos del sistema
    ClearMacros;           //Limpia las macros
    //Inicia PIC
    ExprLevel := 0;  //inicia
    GetHardwareInfo(pic, 'DEFAULT');  //configura modelo por defecto
    pic.ClearMemFlash;
    callResetFlashAndRAM;  {Realmente lo que importa aquí sería limpiar solo la RAM, porque
                        cada procedimiento, reiniciará el puntero de FLASH}
    //Compila el archivo actual como programa o como unidad
    if IsUnit then begin
      CompiledUnit := true;
      //Hay que compilar una unidad
      consoleTickStart;
//      debugln('*** Compiling unit: Pass 1.');
      FirstPass := true;
      CompileUnit(TreeElems.main);
      UpdateCallersToUnits;
      consoleTickCount('** First Pass.');
    end else begin
      //Debe ser un programa
      CompiledUnit := false;
      {Se hace una primera pasada para ver, a modo de exploración, para ver qué
      procedimientos, y variables son realmente usados, de modo que solo estos, serán
      codificados en la segunda pasada. Así evitamos incluir, código innecesario.}
      consoleTickStart;
//      debugln('*** Compiling program: Pass 1.');
      pic.iFlash := 0;     //dirección de inicio del código principal
      FirstPass := true;
      CompileProgram;  //puede dar error
      if HayError then exit;
      UpdateCallersToUnits;
      consoleTickCount('** First Pass.');
      if Link then begin  //El enlazado solo es válido para programas
        {Compila solo los procedimientos usados, leyendo la información del árbol de sintaxis,
        que debe haber sido actualizado en la primera pasada.}
        FirstPass := false;
        CompileLinkProgram;
        consoleTickCount('** Second Pass.');
        //Genera archivo hexa, en la misma ruta del programa
        pic.GenHex(hexFile, ConfigWord);  //CONFIG_NULL;
      end;
    end;
    {-------------------------------------------------}
    lex.ClearAll;//es necesario por dejar limpio
  finally
    ejecProg := false;
    //Tareas de finalización
    if OnAfterCompile<>nil then OnAfterCompile;
  end;
end;
function AdrStr(absAdr: word): string;
{formatea una dirección en cadena.}
begin
  Result := '0x' + IntToHex(AbsAdr, 3);
end;
procedure TCompiler_PIC16.RAMusage(lins: TStrings; varDecType: TVarDecType; ExcUnused: boolean);
{Devuelve una cadena con información sobre el uso de la memoria.}
var
  adStr: String;
  v: TxpEleVar;
  nam, subUsed: string;
  reg: TPicRegister;
  rbit: TPicRegisterBit;
begin
  for v in TreeElems.AllVars do begin   //Se supone que "AllVars" ya se actualizó.
    case varDecType of
    dvtDBDb: begin
      if ExcUnused and (v.nCalled = 0) then continue;
      adStr := v.AddrString;  //dirección hexadecimal
      if adStr='' then adStr := 'XXXX';  //Error en dirección
      if v.typ.IsBitSize then begin
        lins.Add(' ' + v.name + ' Db ' +  adStr);
      end else if v.typ.IsByteSize then begin
        lins.Add(' ' + v.name + ' DB ' +  adStr);
      end else if v.typ.IsWordSize then begin
        lins.Add(' ' + v.name + ' DW ' +  adStr);
      end else begin
        lins.Add(' "' + v.name + '"->' +  adStr);
      end;
    end;
    dvtEQU: begin;
      if ExcUnused and (v.nCalled = 0) then continue;
      if v.nCalled = 0 then subUsed := '; <Unused>' else subUsed := '';
      if v.typ.IsBitSize then begin
        lins.Add('#define ' + v.name + ' ' + AdrStr(v.addr) + ',' +
                                             IntToStr(v.adrBit.bit)+ subUsed);
      end else if v.typ.IsByteSize then begin
        lins.Add(v.name + ' EQU ' +  AdrStr(v.addr)+ subUsed);
      end else if v.typ.IsWordSize then begin
        lins.Add(v.name+'@0' + ' EQU ' +  AdrStr(v.addrL)+ subUsed);
        lins.Add(v.name+'@1' + ' EQU ' +  AdrStr(v.addrH)+ subUsed);
      end else if v.typ.IsDWordSize then begin
        lins.Add(v.name+'@0' + ' EQU ' +  AdrStr(v.addrL)+ subUsed);
        lins.Add(v.name+'@1' + ' EQU ' +  AdrStr(v.addrH)+ subUsed);
        lins.Add(v.name+'@2' + ' EQU ' +  AdrStr(v.addrE)+ subUsed);
        lins.Add(v.name+'@3' + ' EQU ' +  AdrStr(v.addrU)+ subUsed);
      end else begin
        lins.Add('"' + v.name + '"->' +  AdrStr(v.addr) + subUsed);
      end;
    end;
    end;
  end;
  //Reporte de registros de trabajo, auxiliares y de pila
  if (listRegAux.Count>0) or (listRegAuxBit.Count>0) then begin
    lins.Add(';------ Work and Aux. Registers ------');
    for reg in listRegAux do begin
      if not reg.assigned then continue;  //puede haber registros de trabajo no asignados
      nam := pic.NameRAM(reg.addr); //debería tener nombre
      adStr := '0x' + IntToHex(reg.addr, 3);
      lins.Add(nam + ' EQU ' +  adStr);
    end;
    for rbit in listRegAuxBit do begin
      nam := pic.NameRAMbit(rbit.addr, rbit.bit); //debería tener nombre
      adStr := '0x' + IntToHex(rbit.addr, 3);
      lins.Add('#define' + nam + ' ' +  adStr + ',' + IntToStr(rbit.bit));
    end;
  end;
  if (listRegStk.Count>0) or (listRegStkBit.Count>0) then begin
    lins.Add(';------ Stack Registers ------');
    for reg in listRegStk do begin
      nam := pic.NameRAM(reg.addr); //debería tener nombre
      adStr := '0x' + IntToHex(reg.addr, 3);
      lins.Add(nam + ' EQU ' +  adStr);
    end;
    for rbit in listRegStkBit do begin
      nam := pic.NameRAMbit(rbit.addr, rbit.bit); //debería tener nombre
      adStr := '0x' + IntToHex(rbit.addr, 3);
      lins.Add('#define ' + nam + ' ' +  adStr + ',' + IntToStr(rbit.bit));
    end;
  end;
//  lins.Add(';-------------------------');
end;
procedure TCompiler_PIC16.DumpCode(lins: TSTrings; incAdrr, incCom, incVarNam: boolean);
begin
  pic.DumpCode(lins, incAdrr, incCom, incVarNam);
end;
function TCompiler_PIC16.RAMusedStr: string;
var
  usedRAM, totRAM: Word;
begin
  totRAM := pic.TotalMemRAM;
  if totRAM=0 then exit;  //protección
  usedRAM := pic.UsedMemRAM;
  Result := MSG_RAM_USED + IntToStr(usedRAM) +'/'+ IntToStr(totRAM) + 'B (' +
        FloatToStrF(100*usedRAM/totRAM, ffGeneral, 1, 3) + '%)';
end;
function TCompiler_PIC16.FLASHusedStr: string;
var
  totROM: Integer;
  usedROM: Word;
begin
  totROM := pic.MaxFlash;
  usedROM := pic.UsedMemFlash;
  Result := MSG_FLS_USED + IntToStr(usedROM) +'/'+ IntToStr(totROM) + ' (' +
        FloatToStrF(100*usedROM/totROM, ffGeneral, 1, 3) + '%)';
end;
procedure TCompiler_PIC16.GetResourcesUsed(out ramUse, romUse, stkUse: single);
var
  totROM, usedROM: Word;
  usedRAM, totRAM: Word;
begin
  //Calcula RAM
  ramUse := 0;  //valor por defecto
  totRAM := pic.TotalMemRAM;
  if totRAM = 0 then exit;  //protección
  usedRAM := pic.UsedMemRAM;
  ramUse := usedRAM/ totRAM;
  //Calcula ROM
  romUse:= 0;  //valor por defecto
  totROM := pic.MaxFlash;
  if totROM = 0 then exit; //protección
  usedROM := pic.UsedMemFlash;
  romUse := usedROM/totROM;
  //Calcula STACK
  TreeElems.main.UpdateCalledAll;   //Debe haberse llenado TreeElems.main.lstCalled
  //No considera el anidamiento por interrupciones
  stkUse := TreeElems.main.maxNesting/STACK_SIZE;
end;
procedure TCompiler_PIC16.GenerateListReport(lins: TStrings);
{Genera un reporte detallado de la compialción}
var
  curInst, opc: TPIC16Inst;
  i: word;
  OpCodeCoun: array[low(TPIC16Inst)..high(TPIC16Inst)] of integer;
  tmpList: TStringList;
  txt, OpCode, Times, state: String;

  fun: TxpEleFun;
  caller : TxpEleCaller;
  called : TxpElement;
  exitCall: TxpExitCall;
begin
  ////////////////////////////////////////////////////////////
  //////////// Reporte de uso de memeoria  ///////////
  ////////////////////////////////////////////////////////////
  lins.Add(RAMusedStr);
  lins.Add(FLASHusedStr);
  ////////////////////////////////////////////////////////////
  //////////// Reporte de cuenta de instrucciones  ///////////
  ////////////////////////////////////////////////////////////
  //Limpia contadores
  for opc := low(TPIC16Inst) to high(TPIC16Inst) do begin
    OpCodeCoun[opc] := 0;
  end;
  //Cuenta apariciones
  for i:=0 to high(pic.flash) do begin
    if pic.flash[i].used then begin
       pic.PC.W := i;
       curInst := pic.CurInstruction;
       Inc(OpCodeCoun[curInst]);  //Acumula
    end;
  end;
  //Carga en lista para ordenar
  tmpList:= TStringList.Create;
  for opc := low(TPIC16Inst) to high(TPIC16Inst) do begin
    tmpList.Add(Format('%.4d', [OpCodeCoun[Opc]]) + '-' + PIC16InstName[opc]);
  end;
  tmpList.Sort;  //Ordena
  //Muestra lista ordenada
  lins.Add(';INSTRUCTION COUNTER');
  lins.Add(';===================');
  for i:=tmpList.Count-1 downto 0 do begin
    txt := tmpList[i];
    OpCode := copy(txt , 6, 10);
    Times  := copy(txt , 1, 4);
    if Times = '0000' then continue;
    lins.Add(copy(OpCode + '    ',1,7) + '->'+ Times);
  end;
  tmpList.Destroy;

  ////////////////////////////////////////////////////////////
  ////////////////// Reporte de Funciones   ///////////
  ////////////////////////////////////////////////////////////
  lins.Add('');
  lins.Add(';PROCEDURE LIST');
  lins.Add(';===================');

  lins.Add(';NAME                    USED   POSIITON IN SOURCE');
  lins.Add(';----------------------- ------ -------------------------------');
  for fun in TreeElems.AllFuncs do begin
    if fun.nCalled > 0 then begin
      if fun.nCalled = 0 then
        state := 'Unused'
      else
        state := RightStr('     '+IntToStr(fun.nCalled)+ '', 6);

      lins.Add( copy(fun.name + space(24) , 1, 24) + ' ' +
                state + ' ' +
                fun.srcDec.RowColString + ':' + lex.ctxFile(fun.srcDec.idCtx)
      );
    end;
  end;

  ////////////////////////////////////////////////////////////
  ////////////////// Detalle de Funciones   ///////////
  ////////////////////////////////////////////////////////////
  lins.Add('');
  lins.Add(';PROCEDURE DETAIL');
  lins.Add(';===================');
  for fun in TreeElems.AllFuncs do begin
    if fun.nCalled > 0 then begin
      lins.Add('------------------------------------');
      lins.Add('----- PROCEDURE ' + fun.name);
      lins.Add('------------------------------------');
      lins.Add('  Caller Procedures:');
      if fun.lstCallers .Count = 0 then begin
        lins.Add('    <none>');
      end else begin
        for caller in fun.lstCallers do begin
          lins.Add('    - ' + caller.caller.Parent.name);
        end;
      end;
      lins.Add('');

      lins.Add('  Called Procedures:');
      if fun.lstCalled.Count = 0 then begin
        lins.Add('    <none>');
      end else begin
        for called in fun.lstCalled do begin
          lins.Add('    - ' + called.name);
        end;
      end;
      lins.Add('');

      lins.Add('  All Called Procedures:');
      if fun.lstCalledAll.Count = 0 then begin
        lins.Add('    <none>');
      end else begin
        for called in fun.lstCalledAll do begin
          lins.Add('    - ' + called.name);
        end;
      end;
      lins.Add('');

      lins.Add('  Exit Instruction Calls:');
      if fun.lstExitCalls.Count = 0 then begin
        lins.Add('    <none>');
      end else begin
        for exitCall in fun.lstExitCalls do begin
          lins.Add('    - Exit() in ' +exitCall.srcPos.RowColString);
        end;
      end;
      lins.Add('');

    end;
  end;
  //Detalles del programa principal

  lins.Add('------------------------------------');
  lins.Add('----- Main Program');
  lins.Add('------------------------------------');
  lins.Add('  Called Procedures:');
  if TreeElems.main.lstCalled.Count = 0 then begin
    lins.Add('    <none>');
  end else begin
    for called in TreeElems.main.lstCalled do begin
      lins.Add('    - ' + called.name);
    end;
  end;
  lins.Add('');
  //Muestra el máximo nivel de anidamiento.
  lins.Add('Max. Nesting = ' + IntToSTr(TreeElems.main.maxNesting));

end;
constructor TCompiler_PIC16.Create(msg0: TMessageManager);
begin
 // hexFile := 'output.hex';
  inherited Create(msg0);
  lex.OnNewLine:=@cInNewLine;
  mode := modPicPas;   //Por defecto en sintaxis nueva
  StartSyntax;   //Debe hacerse solo una vez al inicio
end;
destructor TCompiler_PIC16.Destroy;
begin
  inherited Destroy;
end;

initialization

finalization

end.
//2645
