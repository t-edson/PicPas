//Main menu
 mnFile.Caption  := Trans('&File'    , '&Archivo'       , '&Khipu'        , '&Datei'		,'Файл'		,'Файл');
 mnEdit.Caption  := Trans('&Edit'    , '&Edición'       , '&Allichay'     , '&Bearbeiten'	,'Зміни'			,'Редактировать');
 mnFind.Caption  := Trans('&Search'  , '&Buscar'        , '&Maskhay'      , '&Suchen','Пошук','Поиск');
 mnView.Caption  := Trans('&View'    , '&Ver'           , '&Qhaway'       , '&Ansicht','Вигляд','Вид');
 mnTools.Caption := Trans('&Tools'   , '&Herramientas'  , '&Llamk''anakuna' , '&Werkzeuge','Інструменти','Инструменты');

//File Actions
 acArcNewProj.Caption := Trans('New &Project', 'Nuevo &Proyecto'  , ''            ,'Neues &Projekt','Новий проект','Новый проект');
 acArcNewFile.Caption := Trans('New &File'      , 'Nuevo &Archivo'   , 'Musuq &Khipu'        ,'&Neu','Новий файл','Новый проект');
 acArcNewFile.Hint    := Trans('New File'       , 'Nuevo Archivo'    , 'Musuq Khipu'         , 'Neue Datei','Новий файл','Новый файл');
 acArcNewProj.Caption := Trans('New &Project'   , 'Nuevo &Proyecto'  , 'Musuq &Proyecto'     ,'Neues &Projekt','Новий проект','Новый файл');
 acArcNewProj.Hint    := Trans('New &Project'   , 'Nuevo Proyecto'   , 'Musuq Proyecto'      ,'Neues &Projekt','Новий проект','Новый проект');
 acArcOpen.Caption    := Trans('&Open...'       , '&Abrir...'        , 'K&ichay'             ,'&Öffnen...','Відкрити...','Открыть...');
 acArcOpen.Hint       := Trans('Open file'      , 'Abrir archivo'    , 'Khiputa kichay'      , 'Datei Öffnen','Відкрити файл','Открыть файл');
 acArcSave.Caption    := Trans('&Save'          , '&Guardar'         , '&Waqaychay'          , '&Speichern','Зберегти','Сохранить');
 acArcSave.Hint       := Trans('Save file'      , 'Guardar archivo'  , 'Khiputa waqaychay'   , 'Datei speichern','Зберегти файл','Сохранить файл');
 acArcSaveAs.Caption  := Trans('Sa&ve As...'    , 'G&uardar Como...' , 'Kay hinata &waqaychay', 'Speichern &unter ...','Зберегти як...','Сохранить как...');
 acArcSaveAs.Hint     := Trans('Save file as...','Guardar como...'  , 'Kay hinata waqaychay','Datei mit unter neuem Namen speichern ...','Зберегти файл як...','Сохранить файл как...');
 acArcCloseFile.Caption:=Trans('&Close File'    , '&Cerrar archivo'  , 'Khiputa wi&sqay'     , 'Datei s&chließen','Закрити файл','Закрыть файл');
 acArcCloseProj.Caption:=Trans('Close Project'  , 'Cerrar Proyecto'  , 'Proyectota wisqay'   , 'Projekt schließen','Закрити проект','Закрыть проект');
 mnSamples.Caption    := Trans('Samples'        , 'Ejemplos'         , 'Qhawarinakuna'       , 'Beispiele','Приклади','Примеры');
 acArcQuit.Caption    := Trans('&Quit'          , '&Salir'           , 'Ll&uqsiy'            , '&Beenden','Вийти','Выход');
 acArcQuit.Hint       := Trans('Close the program','Cerrar el programa','Programata wi&sqay', 'Programm beenden','Закрити програму','Закрыть программу');

//Edit Actions
 acEdUndo.Caption     := Trans('&Undo'       , '&Deshacer'        , '&Paskay', '&Zurück','Відміна','Отмена');
 acEdUndo.Hint        := Trans('Undo'        , 'Deshacer'         , 'Paskay', 'Änderung zurücknehmen','Відміна','Отмена');
 acEdRedo.Caption     := Trans('&Redo'       , '&Rehacer'         , '&Ruwapay', '&Wiederholen','Повторити','Повторить');
 acEdRedo.Hint        := Trans('Redo'        , 'Reahacer'         , 'Ruwapay', 'Änderung wiederholen','Повторити','Повторить');
 acEdCut.Caption      := Trans('C&ut'        , 'Cor&tar'          , 'Ku&chuy', 'A&usschneiden','Вирізати','Вырезать');
 acEdCut.Hint         := Trans('Cut'         , 'Cortar'           , 'Kuchuy', 'Ausschneiden','Вирізати','Вырезать');
 acEdCopy.Caption     := Trans('&Copy'       , '&Copiar'          , 'Kiki&nchay', '&Kopieren','Копіювати','Копировать');
 acEdCopy.Hint        := Trans('Copy'        , 'Copiar'           , 'Kikinchay', 'Kopieren','Копіювати','Копировать');
 acEdPaste.Caption    := Trans('&Paste'      , '&Pegar'           , 'k''ask&ay', '&Einfügen','Вставити','Вставить');
 acEdPaste.Hint       := Trans('Paste'       , 'Pegar'            , 'K''askay', 'Einfügen','Вставити','Вставить');
 acEdSelecAll.Caption := Trans('Select &All'    , 'Seleccionar &Todo'  , 'Llapan&ta Akllay', 'Alles &Auswählen','Вибрати все','Выбрать всё');
 acEdSelecAll.Hint    := Trans('Select all'  , 'Seleccionar todo' , 'Llapanta Akllay', 'Alles auswählen','Вибрати все','Выбрать всё');

//Search Actions
 acSearFind.Caption    := Trans('Find...'      , 'Buscar...'          , 'Maskhay', 'Suchen...','Знайти...','Найти...');
 acSearFind.Hint       := Trans('Find text'    , 'Buscar texto'       , 'Qillqata maskhay', 'Text suchen','Знайти текст','Найти текст');
 acSearFindNxt.Caption := Trans('Find &Next'   , 'Buscar &Siguiente'  , '&Hamuqta Maskhay', 'Weitersuche&n','Знайти наступний','Найти следующий');
 acSearFindNxt.Hint    := Trans('Find Next'    , 'Buscar Siguiente'   , 'Hamuqta Maskhay', 'Nächste Stelle suchen','Знайти наступний','Найти следующий');
 acSearFindPrv.Caption := Trans('Find &Previous','Buscar &Anterior'   , '', '','Знайти попередній','Найти предыдущий');
 acSearFindPrv.Hint    := Trans('Find &Previous','Buscar &Anterior'   , '', '','Знайти попередній','Найти предыдущий');
 acSearReplac.Caption  := Trans('&Replace...'    , '&Reemplazar...'     , '&Yankiy', '&Ersetzen...','Замінити...','Замена...');
 acSearReplac.Hint     := Trans('Replace text'   , 'Reemplazar texto'   , 'Qillqata yankiy', 'Text ersetzen','Замінити текст','Заменить текст');

//View actions
 acViewMsgPan.Caption := Trans('&Messages Panel'         , 'Panel de &Mensajes'           , '&Willanakuna qhawachiq','&Nachrichten Panel','Панель повідомлень','Панель сообщений');
 acViewMsgPan.Hint    := Trans('Show/hide Messages Panel', 'Mostrar/Ocultar el Panel de Mensajes', 'Willanakuna qhawachiqta Rikuchiy/Pakachiy', 'Nachrichten Panel zeigen oder verbergen','Показати/Сховати панель повідомлень','Показать/Спрятать панель сообщений');
 acViewStatbar.Caption:= Trans('&Status Bar'             , 'Barra de &Estado'             , '&Imayna kasqanta Qhawachiq','&Statuszeile','','');
 acViewStatbar.Hint   := Trans('Show/hide Status Bar'    , 'Mostrar/Ocultar la barra de estado', 'Imayna Kasqanta Rikuchiy/Pakachiy','Statuszeile zeigen oder verbergen','','');
 acViewToolbar.Caption:= Trans('&Tool Bar'               , 'Barra de &Herramientas'       , '&Llamk''anakuna Qhawachiq','&Werkzeugleiste','Панель інструментів','Панель инструментов');
 acViewToolbar.Hint   := Trans('Show/hide Tool Bar'      , 'Mostrar/Ocultar la barra de herramientas', 'Llamk''anakuna Qhawachiqta Rikuchiy/Pakachiy', 'Werkzeugleiste zeigen oder verbergen','Показати/Сховати панель інструментів','Показать/Спрятать панель инструментов');
 acViewSynTree.Caption:= Trans('&Code explorer'          , '&Explorador de código.'       , '&Chimpukunata t''aqwiq', '&Quelltext-Explorer','Оглядач кода','Обозреватель кода');
 acViewAsmPan.Caption := Trans('&Assembler Panel'        , '&Panel de ensamblador.'       , '', '','Панель асемблера','Панель ассемблера');


//Tool actions
 acToolCompil.Caption := Trans('&Compile'                , '&Compilar'                    , '&Compilay', '&Compilieren','Компілювати','Компилировать');
 acToolCompil.Hint    := Trans('Compile the source code' , 'Compila el código fuente'     , 'Pachanmanta chimpukuna kaqta compilay', 'Compiliere den Quelltext','Компілювати','Компилировать');
 acToolComEjec.Caption:= Trans('Compile and E&xecute'    , 'Compilar y Ej&ecutar'         , 'Compilay chaymanta &Hinay', 'Compilieren und Au&sführen','Компілювати та виконати','Компилировать и выполнить');
 acToolComEjec.Hint   := Trans('Compile and Execute'     , 'Compilar y Ejecutar'          , 'Compilay chaymanta &Hinay', 'Compilieren und Ausführen','Компілювати та виконати','Компилировать и выполнить');
 acToolPICExpl.Caption:= Trans('PIC E&xplorer'           , 'E&xplorador de PIC'           , 'PIC nisqakunata T''aqwiq', 'PIC E&xplorer','PIC оглядач','PIC обозреватель');
 acToolPICExpl.Hint   := Trans('Open the PIC devices explorer','Abrir el explorador de dispos. PIC', 'Dispos. PIC nisqa t''aqwiqta kichariy','Öffne den PIC Geräte explorer','Відкрити PIC оглядач','Открыть PIC обозреватель');
 acToolASMDebug.Caption  := Trans('ASM &Debugger'        , '&Depurador de ASM'            , '', '','ASM зневаджувач','ASM отладчик');
 acToolASMDebug.Hint     := Trans('ASM &Debugger'        , '&Depurador de ASM'            , '', '','ASM зневаджувач','ASM отладчик');

 acToolListRep.Caption:= Trans('&List Report'            , '&Reporte de listado'          , '', '','Звіт','Отчет');
 acToolConfig.Caption := Trans('&Settings'               , '&Configuración'                , 'Kamachina', '&Einstellungen','Налагодження','Настройки');
 acToolConfig.Hint    := Trans('Settings dialog'         , 'Ver configuración'            , 'Kamachinata qhaway', 'Einstellungs-Dialog','Діалог налагоджень','Диалог настроек');
 acToolFindDec.Caption:= Trans('Find declaration' , 'Ir a la declaración' , 'Riqsichikusqan k''itiman riy', 'Finde Deklaration','Знайти декларування','Найти декларирование');

//Messages
 MSG_MODIF   := Trans('(*)Modified'      , '(*)Modificado'                  , '', '','(*)Змінено','(*)Изменено');
 MSG_SAVED   := Trans('Saved'            , 'Guardado'                       , '', '','Збережено','Сохранено');
 MSG_NOFILES := Trans('No files.'        , 'Sin archivos'                   , '', '','Немає файлів.','Нет файлов.');
 MSG_NOFOUND_:= Trans('No found "%s"'    , 'No se encuentra: "%s"'          , '', '','Не знайдено "%s"','Не найдено "%s"');
 MSG_N_REPLAC:= Trans('%d words replaced', 'Se reemplazaron %d ocurrencias.', '', '','%d слів замінено','%d слов заменено');
 MSG_REPTHIS := Trans('Replace this?'    , '¿Reemplazar esta ocurrencia?'   , '', '','Замінити це?','Заменить это?');


