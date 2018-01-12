var
  ER_ERROR_DIREC, ER_UNKNO_DEVIC, ER_MODE_UNKNOWN, ER_UNKNO_DIREC,
  ER_ERROR_FREQ, ER_IDENT_EXPEC, ER_EXPEC_EQUAL,
  ER_SYNTAX_ERRO, ER_SYNTAX_ERR_: string;
  ER_EXPECTED_BR, ER_ENDIF_NOFOU, ER_UNEXP_ENDIF: String;
  ER_UNEXP_ELSE, ER_CONF_UNDEF_, ER_INVAL_CBIT_: String;
  ER_FILE_NO_FND_, ER_ERIN_NUMBER_, ER_UNKNW_IDENT_: String;
  ER_DIVIDE_ZERO, ER_EVA_ZER_ZER, ER_OPE_NOT_IMP_: String;
  ER_EXPECT_CAR_: String;
  ER_TOOHIGHFRE: String;

procedure SetLanguage;
begin
//Messages when compiling Directives
ER_ERROR_DIREC := trans('Error in directive.'  , 'Error en directiva'      ,'',
                        'Fehler bei Direktive.','Помилка директиви.','Ошибка в директиве.');
ER_UNKNO_DEVIC := trans('Unknown device: %s'   , 'Dispositivo desconocido: %s','',
                        'Unbekanntes Gerät: %s','Невизначений прилад: %s','Неизвестное устройство: %s');
ER_MODE_UNKNOWN:= trans('Mode unknown: %s'     , 'Modo inválido'           , '',
                        'Unbekannter Modus: %s','Невизначений режим: %s','Неизвестный режим: %s');
ER_UNKNO_DIREC := trans('Unknown directive: %s', 'Directiva desconocida: %s','',
                        'Unbekannte Direktive: %s','Невизначена директива: %s','Неизвестная директива: %s');
ER_ERROR_FREQ  := trans('Error in frequency.'  , 'Error en frecuencia.'    , '',
                        'Fehler bei der Frequenz.','Помилка в частоті.','Ошибка в частоте.');
ER_TOOHIGHFRE  := trans('Frequency too high for this device.', 'Frecuencia muy alta para este microproc.', '',
                        ''                                   ,'Частота зависока для цього приладу.','Слишком высокая частота для этого устройства.');
ER_IDENT_EXPEC := trans('Identifier expected.' , 'Identificador esperado.' , '',
                        'Bezeichner erwartet.' ,'Очікується ідентифікатор.','Ожидается идентификатор.');
ER_EXPEC_EQUAL := trans('Expected "="'         , 'Se esperaba "="'         , '',
                        '"=" erwartet.'        , 'Очікується "="','Ожидается "="');
ER_SYNTAX_ERRO := trans('Syntax error.'        , 'Error de sintaxis.'      , '',
                        'Syntaxfehler.'        , 'Помилка синтаксиса.','Ошибка синтаксиса.');
ER_SYNTAX_ERR_ := trans('Syntax error: "%s"'   , 'Error de sintaxis: "%s"' , '',
                        'Syntax-Fehler: "%s"'  , 'Помилка синтаксиса: "%s"','Ошибка синтаксиса: "%s"');
ER_EXPECTED_BR := trans('Expected: "}".'       , 'Se esperaba "}".'        , '',
                        '"}" erwartet.'        , 'Очікується: "}".','Ожидается: "}".');
ER_ENDIF_NOFOU := trans('"$ENDIF" not found.'  , 'No se encontró "$ENDIF".', '',
                        ''                     , '"$ENDIF" не знайдено.','"$ENDIF" не найден.');
ER_UNEXP_ENDIF := trans('Unexpected "$ENDIF".' , 'No se esperaba "$ENDIF".', '',
                        ''                     , 'Непередбачений "$ENDIF".','Непредвиденный "$ENDIF".');
ER_UNEXP_ELSE  := trans('Unexpected "$ELSE".'  , 'No se esperaba "$ELSE".' , '',
                        ''                     , 'Непередбачений "$ELSE".','Непредвиденный "$ELSE".');
ER_CONF_UNDEF_ := trans('Undefined Config. Bit: %s', 'Bit de Config. no definido: %s' , '',
                        ''                       , 'Невизначений біт конфігурації: %s','Неопределённый бит конфигурации: %s');
ER_INVAL_CBIT_ := trans('Invalid Config. Bit: %s', 'Bit de Config. inválido: %s' , '',
                        ''                       , 'Помилковий біт конфігурації: %s','Ошибочный бит конфигурации: %s');
ER_FILE_NO_FND_:= trans('File no found: %s'      , 'Archivo no encontrado: %s' , '',
                        ''                       , 'Файл не знайдено: %s','Файл не найден: %s');
ER_ERIN_NUMBER_:= trans('Error in number: %s'    , 'Error en número: %s' , '',
                        ''                       , 'Помилка в номері: %s','Ошибка в числе: %s');
ER_UNKNW_IDENT_:= trans('Unknown Identifier: %s' , 'Identificador desconocido: %s' , '',
                        ''                       , 'Невідомий ідентифікатор: %s','Неизвестный идентификатор: %s');
ER_DIVIDE_ZERO := trans('Cannot divide by zero'  , 'No se puede dividir por cero' , '',
                        ''                       , 'Не можу поділити на нуль','Не могу поделить на ноль');
ER_EVA_ZER_ZER := trans('Cannot evaluate 0^0'    , 'No se puede evaluar 0^0', '',
                        ''                       , 'Не можу оцінити 0^0', 'Не могу оценить 0^0');
ER_OPE_NOT_IMP_ := trans('Operator not implemented: %s', 'Operador no implementado: %s', '',
                        ''                       , 'Оператор не реалізовано: %s','Оператор не реализован: %s');
ER_EXPECT_CAR_ := trans('Expected "%s"'          , 'Se esperaba "%s"'         , '',
                        '"%s" erwartet.'         , 'Очікується "%s"','Ожидается "%s"');

end;

