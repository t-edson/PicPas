 //Main menu
  mnFile.Caption :=Trans('&File'  , '&Archivo', '');
  mnEdit.Caption :=Trans('&Edit'  , '&Edición', '');
  mnFind.Caption :=Trans('&Search', '&Buscar', '');
  mnView.Caption :=Trans('&View'  , '&Ver', '');
  mnTools.Caption:=Trans('&Tools' , '&Herramientas', '');
  //File Actions
  acArcNewFile.Caption := Trans('New &File'   , 'Nuevo &Archivo', 'Musuq &Khipu');
  acArcNewFile.Hint    := Trans('New File'    , 'Nuevo Archivo', 'Musuq Khipu');
  acArcNewProj.Caption := Trans('New &Project', 'Nuevo &Proyecto', '');
  acArcNewProj.Hint    := Trans('New &Project', 'Nuevo Proyecto', '');
  acArcOpen.Caption    := Trans('&Open...'    , '&Abrir...', '');
  acArcOpen.Hint       := Trans('Open file'   , 'Abrir archivo', '');
  acArcSave.Caption    := Trans('&Save'       , '&Guardar', '');
  acArcSave.Hint       := Trans('Save file'   , 'Guardar archivo', '');
  acArcSaveAs.Caption  := Trans('Sa&ve As...' , 'G&uardar Como...', '');
  acArcSaveAs.Hint     := Trans('Save file as...', 'Guardar como...', '');
  acArcQuit.Caption    := Trans('&Quit'       , '&Salir', '');
  acArcQuit.Hint       := Trans('Close the program','Cerrar el programa', '');
  //Edit Actions
  acEdUndo.Caption     := Trans('&Undo'       , '&Deshacer', '');
  acEdUndo.Hint        := Trans('Undo'        , 'Deshacer', '');
  acEdRedo.Caption     := Trans('&Redo'       , '&Rehacer', '');
  acEdRedo.Hint        := Trans('Redo'        , 'Reahacer', '');
  acEdCut.Caption      := Trans('C&ut'        , 'Cor&tar', '');
  acEdCut.Hint         := Trans('Cut'         , 'Cortar', '');
  acEdCopy.Caption     := Trans('&Copy'       , '&Copiar', '');
  acEdCopy.Hint        := Trans('Copy'        , 'Copiar', '');
  acEdPaste.Caption    := Trans('&Paste'      , '&Pegar', '');
  acEdPaste.Hint       := Trans('Paste'       , 'Pegar', '');
  acEdSelecAll.Caption := Trans('Select &All' , 'Seleccionar &Todo', '');
  acEdSelecAll.Hint    := Trans('Select all'  , 'Seleccionar todo', '');
  acEdModCol.Caption   := Trans('Column mode' , 'Modo Columna', '');
  acEdModCol.Hint      := Trans('Column mode' , 'Modo columna', '');
  //Search Actions
  acBusBuscar.Caption  := Trans('Search...'   , 'Buscar...', '');
  acBusBuscar.Hint     := Trans('Search text' , 'Buscar texto', '');
  acBusBusSig.Caption  := Trans('Search &Next', 'Buscar &Siguiente', '');
  acBusBusSig.Hint     := Trans('Search Next' , 'Buscar Siguiente', '');
  acBusReemp.Caption   := Trans('&Replace...' , '&Remplazar...', '');
  acBusReemp.Hint      := Trans('Replace text', 'Reemplazar texto', '');


  acViewMsgPan.Caption := Trans('&Messages Panel', 'Panel de &Mensajes', '');
  acViewMsgPan.Hint    := Trans('Show/hide the Messages Panel','Mostrar u Ocultar el Panel de Mensajes', '');
  acViewStatbar.Caption:= Trans('&Status Bar'    , 'Barra de &Estado', '');
  acViewStatbar.Hint   := Trans('Show o hide the Status Bar','Mostrar u Ocultar la barra de estado', '');
  acViewToolbar.Caption:= Trans('&Tool Bar'      , 'Barra de &Herramientas', '');
  acViewToolbar.Hint   := Trans('Show/hide the Tool Bar', 'Mostrar u Ocultar la barra de herramientas', '');

  acToolCompil.Caption:= Trans('&Compile'               , '&Compilar', '');
  acToolCompil.Hint:=    Trans('Compile the source code', 'Compila el código fuente', '');
  acToolComEjec.Caption:=Trans('Compile and E&xecute'   , 'Compilar y Ej&ecutar', '');
  acToolComEjec.Hint:=   Trans('Compile and Execute'    , 'Compilar y Ejecutar', '');
  acToolPICExpl.Caption:=Trans('PIC E&xplorer'          , 'E&xplorador de PIC', '');
  acToolPICExpl.Hint:=   Trans('Open the PIC devices explorer','Abre el explorador de dispositivos PIC', '');
  acToolConfig.Caption:= Trans('&Settings'              , 'Configuración', '');
  acToolConfig.Hint :=   Trans('Settings dialog'        , 'Ver configuración', '');
