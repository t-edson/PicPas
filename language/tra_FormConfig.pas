var
  TIT_CFG_ENVIRON, TIT_CFG_MESPAN, TIT_CFG_CODEXP,
  TIT_CFG_EDITOR, TIT_CFG_SYNTAX,
  TIT_CFG_ASSEMB,  TIT_CFG_COMPIL, TIT_CFG_EXTOOL: String;
  LABEL_THEM_NONE, TIT_CFG_EDICOL: String;

procedure TConfig.SetLanguage;
begin
  fraCfgSynEdit.SetLanguage;
  fraCfgExtTool.SetLanguage;
  fraCfgSyntax.SetLanguage;

Caption              := Trans('Settings'               , 'Configuración'            , '',
                              'Einstellungen','Налаштування','Настройки');
BitAceptar.Caption   := Trans('&OK'                    , 'Aceptar'                  , '',
                              '&Ok','','');
BitAplicar.Caption   := Trans('&Apply'                 , 'Aplicar'                  , '',
                              '&Übernehmen','Застосувати','Применить');
BitCancel.Caption    := Trans('&Cancel'                , 'Cancelar'                  , '',
                              '&Abbrechen','Відміна','Отмена');

////////////////////////////////////////////////////////////////////////////
//////////////////////////  Environment Settings //////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_ENVIRON     := Trans('Environment', 'Entorno', '',
                             'Umgebung','Оточення','Окружение');

Label2.Caption      := Trans('Language'               , 'Lenguaje'                 , '',
                              'Sprache','Мова','Язык');
RadioGroup1.Caption := Trans('Toolbar'                 , 'Barra de herramientas'    , '',
                              'Werkzeugleiste','Панель інструментів','Панель инструментов');
RadioGroup1.Items[0]:= Trans('Small Icons'             , 'Íconos pequeños'          , '',
                              'Kleine Bilder','Маленькі піктограми','Маленькие иконки');
RadioGroup1.Items[1]:= Trans('Big Icons'               , 'Íconos grandes'           , '',
                              'Große Bilder','Великі піктограми','Большие иконки');
label1.Caption      := Trans('Units Path:'             , 'Ruta de unidades'         , '',
                              'Unitpfad:','','');

label3.Caption      := Trans('&Set Theme'        , '&Fijar Tema', '',
                               '','Обрати тему','Выбрать тему');
LABEL_THEM_NONE     := Trans('None', 'Ninguno', '',
                             '','Нічого','Ничего');
label4.Caption      := Trans('&Create Theme'        , '&Crear Tema', '',
                               '','Створити тему','Создать тему');
butSaveCurThem.Caption := Trans('&Save current config.', 'Guardar config. actual', '',
                             '','Зберегти налаштування','Сохранить настройки');;

chkLoadLast.Caption := Trans('Load last file edited'     , 'Cargar último archivo editado', '',
                             'Letzte editierte Datei laden','Завантажити останній файл','Загрузить последний файл');

lblPanelCol.Caption := Trans('Panels Color:'             , 'Color de los paneles:', '',
                               'Paneelenfarbe:','Колір панелей:','Цвет панелей:');
lblSplitCol.Caption := Trans('Splitters color:'          , 'Color de los separadores:', '',
                               'Trenner-Farbe:','Колір розподілувача:','Цвет разделителя:');

////////////////////////////////////////////////////////////////////////////
//////////////////////////  Code Explorer //////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_CODEXP    := Trans('Code Explorer', 'Explorador de Código', '',
                           '','Оглядач коду','Инспектор кода');
lblCodExplCol1.Caption:= Trans('Back color:' , 'Color de Fondo:', '',
                               'Hintergrundfarbe:','Колір фону:','Цвет фона:');
lblCodExplCol2.Caption:= Trans('Text Color:' , 'Color de Texto:', '',
                               'Textfarbe:','Колір тексту:','Цвет текста:');
grpFilType.Caption    := Trans('File types shown:' , 'Tipos de archivos mostrados:', '',
                               '','','');
////////////////////////////////////////////////////////////////////////////
//////////////////////////  Message Panel //////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_MESPAN    := Trans('Message Panel', 'Panel de Mensajes', '',
                           '','Панель повідомлень','Панель сообщений');
lblMessPan1.Caption   := Trans('Back color'   , 'Color de Fondo', '',
                               'Hintergrundfarbe','Колір фону','Цвет фона');
lblMessPan2.Caption   := Trans('Text color:'  , 'Color de Texto', '',
                               'Textfarbe:','Колір тексту:','Цвет текста:');
lblMessPan3.Caption   := Trans('Error color:' , 'Color de Error', '',
                               'Fehlerfarbe:','Колір помилки:','Цвет ошибки:');
lblMessPan4.Caption   := Trans('Selection color:', 'Color de Selección', '',
                               'Auswahlfarbe:','Колір обраного:','Цвет выделения:');

////////////////////////////////////////////////////////////////////////////
//////////////////////////  Editor settings ///////////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_EDITOR    := Trans('Editor'                 , 'Editor'                   , '',
                              'Editor','Редактор','Редактор');

Label6.Caption       := trans('&Font:'                 , '&Letra:'                     ,'',
                              'Schri&ftart:','Шрифт:','Шрифт:');
Label7.Caption       := trans('&Size:'                 , '&Tamaño:'                    ,'',
                              '&Größe:','Розмір:','Размер:');
chkViewVScroll.Caption:= trans('&Vertical Scrollbar'    , 'Barra de desplaz &Vert.'     ,'',
                              '& Vertikale Bildlaufleiste','Вертикальній скролбар','Вертикальный скролбар');
chkViewHScroll.Caption:= trans('&Horizontal Scrollbar'  , 'Barra de desplaz &Horiz.'    ,'',
                              '&Horizontale Bildlaufleiste','Горизонтальний скролбар','Горизонтальный скролбар');

grpTabEdiState.Caption :=Trans('Tab Editor State'  , 'Estado de pestañas del editor', '',
                              'Registerkarte Editor Zustand','','');;
grpTabEdiState.Items[0]:=Trans('&Show always'      , 'Mostrar &Siempre'         , '',
                              '','Показувати завжди','Показывать всегда');
grpTabEdiState.Items[1]:=Trans('Hide for &One file', '&Ocultar si hay un archivo', '',
                              'Ausblenden für &eine Datei','Сховати для одного файлу','Скрыть для одного файла');
grpTabEdiState.Items[2]:=Trans('&Hide always'      , 'Ocultar &Siempre'          , '',
                              '&Immer ausblenden','Ховати завжди','Прятать всегда');

chkAutSynChk.Caption := Trans('Automatic Syntax checking', 'Verificac. Automática de sintaxis', '',
                              'Automatische Syntaxprüfung','Автомтична перевірка синтаксису','Автоматическая проверка синтаксиса');

////////////////////////////////////////////////////////////////////////////
//////////////////////////// Editor Colors Settings ////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_EDICOL    := Trans('Colors'                 , 'Colores'                   , '',
                           '','Кольори','Цвета');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// Editor-Syntax Settings ////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_SYNTAX    := Trans('Syntax'                 , 'Sintaxis'                 , '',
                              'Syntax','Синтакс','Синтакс');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// Assembler settings ////////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_ASSEMB    := Trans('Assembler'              , 'Ensamblador'              , '',
                              'Assembler','Асемблер','Ассемблер');
chkIncHeadMpu.Caption:= Trans('Include MPU &Header'    , 'Incluir &Encabezado de MPU','',
                              'MPU &Kopfzeilen einbinden','Включити MPU заголовок','Включить MPU заголовок');
chkIncDecVar.Caption := Trans('Include &Variables declaration', 'Incluir Declaración de variables', '',
                              'Variablendeklaration einfügen','Включити декларування змінних','Включить объявление переменных');
RadioGroup2.Caption  := Trans('Style'                  , 'Estilo'                   , '',
                              'Stil','Стиль','Стиль');
chkExcUnused.Caption   := Trans('Exclude unused'         , 'Excluir no usadas'        , '',
                              'Unbenutzte ausschlieÃŸen','Виключити невикористовуване','Исключить неиспользуемое');
chkIncAddress.Caption:= Trans('Include &Memory Address','Incluir &Dirección de memoria','',
                              'Speicheradressen einbinden','Включити Memory Address','Включить Memory Address');
chkIncComment.Caption:= Trans('Include &Comments'      , 'Incluir &Comentarios'     , '',
                              'Kommentare hinzufügen','Включити коментарі','Включить комментарии');
chkIncComment2.Caption:=Trans('Include &Detailed comments', 'Incluir Comentarios &detallados' , '',
                              '&Detaillierte Kommentare hinzufügen','Включити детальні коментарі','Включить детальные комментарии');
chkIncVarName.Caption:= Trans('Include &Variable Names','Incluir Nombre de &Variables','',
                              '&Variablennamen einbinden','Включити імена змінних','Включить имена переменных');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// Output Settings ///////////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_COMPIL    := Trans('Compiler'               , 'Compilador'               , '',
                              'Compiler','Компілятор','Компилятор');
chkShowErrMsg.Caption:= Trans('&Show Error Messages'   , '&Mostrar mensajes de error', '',
                              '&Zeige Fehlermeldungen','Показувати сповіщення про помилки','Показывать сообщения об ошибках');
grpOptimLev.Caption  := Trans('Optimization Level:'    , 'Nivel de optimización:'   , '',
                              'Optimierungsstufe:','Рівень оптимізації:','Уровень оптимизации:');
grpOptimLev.Items[0] := Trans('Fool'                   , 'Tonto'                    , '',
                              'Dumm','Дурень','Дурак');
grpOptimLev.Items[1] := Trans('Smart'                  , 'Inteligente'              , '',
                              'Schlau','Розумний','Умный');
chkOptBnkAftIF.Caption := Trans('After IF structure'    , 'Después de instrucciones IF.', '',
                               'Nach IF-Struktur','Після структури IF','После структуры IF');
chkOptBnkBefPro.Caption:= Trans('Before Procedures'     , 'Antes de procedimientos.', '',
                               'Vor Prozeduren','Перед процедурами','Перед процедурами');
chkOptBnkAftPro.Caption:= Trans('After Procedures'      , 'Después de procedimientos.', '',
                                'Nach den Prozeduren','Після адреси','После адреса');
chkReuProcVar.Caption := Trans('Reuse Procedures Variables', 'Reutilizar variables de proced.', '',
                               '','Повторно використовувати змінні процедур','Повторно использовать переменные процедур');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// External Tool ////////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_EXTOOL    := Trans('External Tool'           , 'Herramienta Externa'      , '',
                               '','Завнішній інструмент','Внешний инструмент');
FillTree;
end;

