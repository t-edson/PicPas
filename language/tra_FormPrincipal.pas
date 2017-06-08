//Main menu
 mnFile.Caption  := Trans('&File'    , '&Archivo'       , '&Khipu'        , '&Datei');
 mnEdit.Caption  := Trans('&Edit'    , '&Edición'       , '&Allichay'     , '&Bearbeiten');
 mnFind.Caption  := Trans('&Search'  , '&Buscar'        , '&Maskhay'      , '&Suchen');
 mnView.Caption  := Trans('&View'    , '&Ver'           , '&Qhaway'       , '&Ansicht');
 mnTools.Caption := Trans('&Tools'   , '&Herramientas'  , '&Llamkanakuna' , '&Werkzeuge');
 //File Actions
 acArcNewProj.Caption := Trans('New &Project', 'Nuevo &Proyecto'  , ''            ,'Neues &Projekt');
 acArcNewFile.Caption := Trans('New &File'      , 'Nuevo &Archivo'   , 'Musuq &Khipu'        ,'&Neu');
 acArcNewFile.Hint    := Trans('New File'       , 'Nuevo Archivo'    , 'Musuq Khipu'         , 'Neue Datei');
 acArcNewProj.Caption := Trans('New &Project'   , 'Nuevo &Proyecto'  , 'Musuq &Proyecto'     ,'Neues &Projekt');
 acArcNewProj.Hint    := Trans('New &Project'   , 'Nuevo Proyecto'   , 'Musuq Proyecto'      ,'Neues &Projekt');
 acArcOpen.Caption    := Trans('&Open...'       , '&Abrir...'        , 'K&ichay'             ,'&Öffnen...');
 acArcOpen.Hint       := Trans('Open file'      , 'Abrir archivo'    , 'Khiputa kichay'      , 'Datei öffnen');
 acArcSave.Caption    := Trans('&Save'          , '&Guardar'         , '&Waqaychay'          , '&Speichern');
 acArcSave.Hint       := Trans('Save file'      , 'Guardar archivo'  , 'Khiputa waqaychay'   , 'Datei speichern');
 acArcSaveAs.Caption  := Trans('Sa&ve As...'    , 'G&uardar Como...' , 'Kay hinata &waqaychay', 'Speichern &unter ...');
 acArcSaveAs.Hint     := Trans('Save file as...','Guardar como...'  , 'Kay hinata waqaychay','Datei mit unter neuem Namen speichern ...');
 acArcCloseFile.Caption:=Trans('&Close File'    , '&Cerrar archivo'  , 'Khiputa wi&sqay'     , 'Datei s&chließen');
 acArcCloseProj.Caption:=Trans('Close Project'  , 'Cerrar Proyecto'  , 'Proyectota wisqay'   , 'Projekt schließen');
 mnSamples.Caption    := Trans('Samples'        , 'Ejemplos'         , 'Qhawarinakuna'       , 'Beispiele');
 acArcQuit.Caption    := Trans('&Quit'          , '&Salir'           , 'Ll&uqsiy'            , '&Beenden');
 acArcQuit.Hint       := Trans('Close the program','Cerrar el programa','Programata wi&sqay', 'Programm beenden');
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
 acEdSelecAll.Caption := Trans('Select &All'    , 'Seleccionar &Todo'  , '', 'Alles &Auswählen');
 acEdSelecAll.Hint    := Trans('Select all'  , 'Seleccionar todo' , '', 'Alles auswählen');
 //Search Actions
 acBusFind.Caption    := Trans('Search...'      , 'Buscar...'          , '', 'Suchen...');
 acBusFind.Hint       := Trans('Search text'    , 'Buscar texto'       , '', 'Text suchen');
 acBusFindNxt.Caption := Trans('Search &Next'   , 'Buscar &Siguiente'  , '', 'Weitersuche&n');
 acBusFindNxt.Hint    := Trans('Search Next'    , 'Buscar Siguiente'   , '', 'Nächste Stelle suchen');
 acBusReplac.Caption  := Trans('&Replace...'    , '&Reemplazar...'     , '', '&Ersetzen...');
 acBusReplac.Hint     := Trans('Replace text'   , 'Reemplazar texto'   , '', 'Text ersetzen');
 //View actions
 acViewMsgPan.Caption := Trans('&Messages Panel'         , 'Panel de &Mensajes'           , '','&Nachrichten Panel');
 acViewMsgPan.Hint    := Trans('Show/hide Messages Panel' , 'Mostrar/Ocultar el Panel de Mensajes', '', 'Nachrichten Panel zeigen oder verbergen');
 acViewStatbar.Caption:= Trans('&Status Bar'             , 'Barra de &Estado'             , '','&Statuszeile');
 acViewStatbar.Hint   := Trans('Show/hide Status Bar'     , 'Mostrar/Ocultar la barra de estado', '','Statuszeile zeigen oder verbergen');
 acViewToolbar.Caption:= Trans('&Tool Bar'               , 'Barra de &Herramientas'       , '','&Werkzeugleiste');
 acViewToolbar.Hint   := Trans('Show/hide Tool Bar'       , 'Mostrar/Ocultar la barra de herramientas', '', 'Werkzeugleiste zeigen oder verbergen');
 acViewSynTree.Caption:= Trans('&Code explorer'           , '&Explorador de código.'       , '', '&Quelltext-Explorer');

 acToolCompil.Caption := Trans('&Compile'                 , '&Compilar'                    , '', '&Compilieren');
 acToolCompil.Hint   := Trans('Compile the source code'  , 'Compila el código fuente'     , '', 'Compiliere den Quelltext');
 acToolComEjec.Caption:= Trans('Compile and E&xecute'     , 'Compilar y Ej&ecutar'         , '', 'Compilieren und Au&sführen');
 acToolComEjec.Hint  := Trans('Compile and Execute'      , 'Compilar y Ejecutar'          , '', 'Compilieren und Ausführen');
 acToolPICExpl.Caption:= Trans('PIC E&xplorer'            , 'E&xplorador de PIC'           , '', 'PIC E&xplorer');
 acToolPICExpl.Hint   := Trans('Open the PIC devices explorer','Abrir el explorador de dispos. PIC', '','Öffne den PIC Geräte explorer');
 acToolConfig.Caption := Trans('&Settings'                , 'Configuración'                , '', '&Einstellungen');
 acToolConfig.Hint   := Trans('Settings dialog'             , 'Ver configuración'            , '', 'Einstellungs-Dialog');
