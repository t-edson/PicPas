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
                        'Fehler bei Direktive.',
                        '#uk=',
                        '#ru=');
ER_UNKNO_DEVIC := trans('Unknown device: %s'   , 'Dispositivo desconocido: %s','',
                        'Unbekanntes Gerät: %s',
                        '#uk=',
                        '#ru=');
ER_MODE_UNKNOWN:= trans('Mode unknown: %s'     , 'Modo inválido'           , '',
                        'Unbekannter Modus: %s',
                        '#uk=',
                        '#ru=');
ER_UNKNO_DIREC := trans('Unknown directive: %s', 'Directiva desconocida: %s','',
                        'Unbekannte Direktive: %s',
                        '#uk=',
                        '#ru=');
ER_ERROR_FREQ  := trans('Error in frequency.'  , 'Error en frecuencia.'    , '',
                        'Fehler bei der Frequenz.',
                        '#uk=',
                        '#ru=');
ER_TOOHIGHFRE  := trans('Frequency too high for this device.', 'Frecuencia muy alta para este microproc.', '',
                        '',
                        '#uk=',
                        '#ru=');
ER_IDENT_EXPEC := trans('Identifier expected.' , 'Identificador esperado.' , '',
                        'Bezeichner erwartet.',
                        '#uk=',
                        '#ru=');
ER_EXPEC_EQUAL := trans('Expected "="'         , 'Se esperaba "="'         , '',
                        '"=" erwartet.',
                        '#uk=',
                        '#ru=');
ER_SYNTAX_ERRO := trans('Syntax error.'        , 'Error de sintaxis.'      , '',
                        'Syntaxfehler.',
                        '#uk=',
                        '#ru=');
ER_SYNTAX_ERR_ := trans('Syntax error: "%s"'   , 'Error de sintaxis: "%s"' , '',
                        'Syntax-Fehler: "%s"',
                        '#uk=',
                        '#ru=');
ER_EXPECTED_BR := trans('Expected: "}".'       , 'Se esperaba "}".'        , '',
                        '"}" erwartet.',
                        '#uk=',
                        '#ru=');
ER_ENDIF_NOFOU := trans('"$ENDIF" not found.'  , 'No se encontró "$ENDIF".', '',
                        '',
                        '#uk=',
                        '#ru=');
ER_UNEXP_ENDIF := trans('Unexpected "$ENDIF".' , 'No se esperaba "$ENDIF".', '',
                        '',
                        '#uk=',
                        '#ru=');
ER_UNEXP_ELSE  := trans('Unexpected "$ELSE".'  , 'No se esperaba "$ELSE".' , '',
                        '',
                        '#uk=',
                        '#ru=');
ER_CONF_UNDEF_ := trans('Undefined Config. Bit: %s', 'Bit de Config. no definido: %s' , '',
                        '',
                        '#uk=',
                        '#ru=');
ER_INVAL_CBIT_ := trans('Invalid Config. Bit: %s', 'Bit de Config. inválido: %s' , '',
                        '',
                        '#uk=',
                        '#ru=');
ER_FILE_NO_FND_:= trans('File no found: %s'      , 'Archivo no encontrado: %s' , '',
                        '',
                        '#uk=',
                        '#ru=');
ER_ERIN_NUMBER_:= trans('Error in number: %s'    , 'Error en número: %s' , '',
                        '',
                        '#uk=',
                        '#ru=');
ER_UNKNW_IDENT_:= trans('Unknown Identifier: %s' , 'Identificador desconocido: %s' , '',
                        '',
                        '#uk=',
                        '#ru=');
ER_DIVIDE_ZERO := trans('Cannot divide by zero' , 'No se puede dividir por cero' , '',
                        '',
                        '#uk=',
                        '#ru=');
ER_EVA_ZER_ZER := trans('Cannot evaluate 0^0'   , 'No se puede evaluar 0^0', '',
                        '',
                        '#uk=',
                        '#ru=');
ER_OPE_NOT_IMP_ := trans('Operator not implemented: %s', 'Operador no implementado: %s', '',
                        '',
                        '#uk=',
                        '#ru=');
ER_EXPECT_CAR_ := trans('Expected "%s"'           , 'Se esperaba "%s"'         , '',
                        '"%s" erwartet.',
                        '#uk=',
                        '#ru=');

end;
