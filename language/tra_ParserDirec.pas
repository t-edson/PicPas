var
  ER_ERROR_DIREC, ER_UNKNO_DEVIC, ER_MODE_UNKNOWN, ER_UNKNO_DIREC,
  ER_ERROR_FREQ, ER_IDENT_EXPEC, ER_EXPEC_EQUAL,
  ER_SYNTAX_ERRO, ER_SYNTAX_ERR_: string;
  ER_EXPECTED_BR, ER_ENDIF_NOFOU, ER_UNEXP_ENDIF: String;
  ER_UNEXP_ELSE, ER_CONF_UNDEF_, ER_INVAL_CBIT_: String;
  ER_FILE_NO_FND_: String;

procedure SetLanguage(idLang: string);
begin
  curLang := idLang;
//Messages when compiling Directives
ER_ERROR_DIREC := trans('Error in directive.'  , 'Error en directiva'      ,'',
                        'Fehler bei Direktive.');
ER_UNKNO_DEVIC := trans('Unknown device: %s'   , 'Dispositivo desconocido: %s','',
                        'Unbekanntes Gerät: %s');
ER_MODE_UNKNOWN:= trans('Mode unknown: %s'     , 'Modo inválido'           , '',
                        'Unbekannter Modus: %s');
ER_UNKNO_DIREC := trans('Unknown directive: %s', 'Directiva desconocida: %s','',
                        'Unbekannte Direktive: %s');
ER_ERROR_FREQ  := trans('Error in frequency.'  , 'Error en frecuencia.'    , '',
                        'Fehler bei der Frequenz.');
ER_IDENT_EXPEC := trans('Identifier expected.' , 'Identificador esperado.' , '',
                        'Bezeichner erwartet.');
ER_EXPEC_EQUAL := trans('Expected "="'         , 'Se esperaba "="'         , '',
                        '"=" erwartet.');
ER_SYNTAX_ERRO := trans('Syntax error.'        , 'Error de sintaxis.'      , '',
                        'Syntaxfehler.');
ER_SYNTAX_ERR_ := trans('Syntax error: "%s"'   , 'Error de sintaxis: "%s"' , '',
                        'Syntax-Fehler: "%s"');
ER_EXPECTED_BR := trans('Expected: "}".'       , 'Se esperaba "}".'        , '',
                        '');
ER_ENDIF_NOFOU := trans('"$ENDIF" not found.'  , 'No se encontró "$ENDIF".', '',
                        '');
ER_UNEXP_ENDIF := trans('Unexpected "$ENDIF".' , 'No se esperaba "$ENDIF".', '',
                        '');
ER_UNEXP_ELSE  := trans('Unexpected "$ELSE".'  , 'No se esperaba "$ELSE".' , '',
                        '');
ER_CONF_UNDEF_ := trans('Undefined Config. Bit: %s', 'Bit de Config. no definido: %s' , '',
                        '');
ER_INVAL_CBIT_ := trans('Invalid Config. Bit: %s', 'Bit de Config. inválido: %s' , '',
                        '');
ER_FILE_NO_FND_:= trans('File no found: %s'      , 'Archivo no encontrado: %s' , '',
                        '');
end;
