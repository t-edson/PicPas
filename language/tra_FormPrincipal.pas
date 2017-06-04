//Main menu
 mnFile.Caption :=Trans('&File'  , '&Archivo'     , '', '&Datei');
 mnEdit.Caption :=Trans('&Edit'  , '&Edición'     , '', '&Bearbeiten');
 mnFind.Caption :=Trans('&Search', '&Buscar'      , '', '&Suchen');
 mnView.Caption :=Trans('&View'  , '&Ver'         , '', '&Ansicht');
 mnTools.Caption:=Trans('&Tools' , '&Herramientas', '','&Werkzeuge');
 //File Actions
 acArcNewFile.Caption := Trans('New &File'   , 'Nuevo &Archivo'   , 'Musuq &Khipu','&Neu');
 acArcNewFile.Hint    := Trans('New File'    , 'Nuevo Archivo'    , 'Musuq Khipu' ,'Neue Datei');
 acArcNewProj.Caption := Trans('New &Project', 'Nuevo &Proyecto'  , ''            ,'');
 acArcNewProj.Hint    := Trans('New &Project', 'Nuevo Proyecto'   , ''            ,'');
 acArcOpen.Caption    := Trans('&Open...'    , '&Abrir...'        , '&Kichay'     ,'&Öffnen...');
 acArcOpen.Hint       := Trans('Open file'   , 'Abrir archivo'    , ''            , 'Datei öffnen');
 acArcSave.Caption    := Trans('&Save'       , '&Guardar'         , ''            , '&Speichern');
 acArcSave.Hint       := Trans('Save file'   , 'Guardar archivo'  , ''            , 'Datei speichern');
 acArcSaveAs.Caption  := Trans('Sa&ve As...' , 'G&uardar Como...' , ''            , 'Speichern &unter ...');
 acArcSaveAs.Hint     := Trans('Save file as...', 'Guardar como...',''            ,'Datei mit unter neuem Namen speichern ...');
 acArcCloseFile.Caption:=Trans('&Close File' , '&Cerrar archivo'  , ''            , '');
 acArcCloseProj.Caption:=Trans('Close Project', 'Cerrar Proyecto' , ''            , '');
 mnSamples.Caption    := Trans('Samples'     , 'Ejemplos'         , ''            , '');
 acArcQuit.Caption    := Trans('&Quit'       , '&Salir'           , ''            , '&Beenden');
 acArcQuit.Hint       := Trans('Close the program','Cerrar el programa', ''       , 'Programm beenden'); 
 //Edit Actions
 acEdUndo.Caption     := Trans('&Undo'       , '&Deshacer'        , '', '&Zurück');
 acEdUndo.Hint        := Trans('Undo'        , 'Deshacer'         , '', 'Änderung zurücknehmen');
 acEdRedo.Caption     := Trans('&Redo'       , '&Rehacer'         , '', '&Wiederholen');
 acEdRedo.Hint        := Trans('Redo'        , 'Reahacer'         , '', 'Änderung wiederholen');
 acEdCut.Caption      := Trans('C&ut'        , 'Cor&tar'          , '', 'A&usschneiden');
 acEdCut.Hint         := Trans('Cut'         , 'Cortar'           , '', 'Ausschneiden');
 acEdCopy.Caption     := Trans('&Copy'       , '&Copiar'          , '', '&Kopieren');
 acEdCopy.Hint        := Trans('Copy'        , 'Copiar'           , '', 'Kopieren');
 acEdPaste.Caption    := Trans('&Paste'      , '&Pegar'           , '', '&Einfügen');
 acEdPaste.Hint       := Trans('Paste'       , 'Pegar'            , '', 'Einfügen');
 acEdSelecAll.Caption := Trans('Select &All' , 'Seleccionar &Todo', '', 'Alles &Auswählen');
 acEdSelecAll.Hint    := Trans('Select all'  , 'Seleccionar todo' , '', 'Alles auswählen');
 //Search Actions
 acBusBuscar.Caption  := Trans('Search...'   , 'Buscar...'        , '','Suchen...');
 acBusBuscar.Hint     := Trans('Search text' , 'Buscar texto'     , '','Text suchen');
 acBusBusSig.Caption  := Trans('Search &Next', 'Buscar &Siguiente', '','Weitersuche&n');
 acBusBusSig.Hint     := Trans('Search Next' , 'Buscar Siguiente' , '','Nächste Stelle suchen');
 acBusReemp.Caption   := Trans('&Replace...' , '&Reemplazar...'   , '','&Ersetzen...');
 acBusReemp.Hint      := Trans('Replace text', 'Reemplazar texto' , '','Text ersetzen');
 //View actions
 acViewMsgPan.Caption := Trans('&Messages Panel'         , 'Panel de &Mensajes'           , '','&Nachrichten Panel');
 acViewMsgPan.Hint    := Trans('Show/hide the Messages Panel','Mostrar u Ocultar el Panel de Mensajes', '','Nachrichten Panel zeigen oder verbergen');
 acViewStatbar.Caption:= Trans('&Status Bar'             , 'Barra de &Estado'             , '','&Statuszeile');
 acViewStatbar.Hint   := Trans('Show o hide the Status Bar','Mostrar u Ocultar la barra de estado', '','Statuszeile zeigen oder verbergen');
 acViewToolbar.Caption:= Trans('&Tool Bar'               , 'Barra de &Herramientas'       , '','&Werkzeugleiste');
 acViewToolbar.Hint   := Trans('Show/hide the Tool Bar'  , 'Mostrar u Ocultar la barra de herramientas', '','Werkzeugleiste zeigen oder verbergen');
 acViewSynTree.Caption:= Trans('&Code explorer'          , '&Explorador de código.'       , '','&Quelltext-Explorer');

 acToolCompil.Caption:= Trans('&Compile'                 , '&Compilar'                    , '', '&Compilieren');
 acToolCompil.Hint   := Trans('Compile the source code'  , 'Compila el código fuente'     , '', 'Compiliere den Quelltext');
 acToolComEjec.Caption:=Trans('Compile and E&xecute'     , 'Compilar y Ej&ecutar'         , '', 'Compilieren und Au&sführen');
 acToolComEjec.Hint  := Trans('Compile and Execute'      , 'Compilar y Ejecutar'          , '', 'Compilieren und Ausführen');
 acToolPICExpl.Caption:=Trans('PIC E&xplorer'            , 'E&xplorador de PIC'           , '', 'PIC E&xplorer');
 acToolPICExpl.Hint  := Trans('Open the PIC devices explorer','Abre el explorador de dispos. PIC', '','Öffne den PIC Geräte explorer');
 acToolConfig.Caption:= Trans('&Settings'                , 'Configuración'                , '', '&Einstellungen');
 acToolConfig.Hint   := Trans('Settings dialog'             , 'Ver configuración'            , '', 'Einstellungs-Dialog');
