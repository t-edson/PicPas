//ASM blocks errors
ER_EXPEC_COMMA := trans('Expected ",".'                  , 'Se esperaba ","'                ,'',
                        '"," erwartet.',
                        '#uk=',
                        '#ru=');
ER_EXP_ADR_VAR := trans('Expected address or variable name.','Se esperaba dirección o variable.','',
                        'Adresse oder Variablenname erwartete.',
                        '#uk=',
                        '#ru=');
ER_EXP_CON_VAL := trans('Expected constant or value.'    ,'Se esperaba constante o variable.','',
                        'Konstante oder Wert erwartet.',
                        '#uk=',
                        '#ru=');
ER_NOGETADD_VAR:= trans('Cannot get address of this Variable', 'No se puede obtener la dirección de esta variable.', '',
                        'Kann Adresse dieser Variable nicht ermitteln',
                        '#uk=',
                        '#ru=');
ER_NOGETVAL_CON:= trans('Cannot get value of this costant', 'No se puede obtener el valor de esta constante.', '',
                        'Kann den Wert dieser Konstanten nicht ermitteln',
                        '#uk=',
                        '#ru=');
ER_INV_ASMCODE := trans('Invalid ASM Opcode: %s'         , 'Instrucción inválida: %s'       ,'',
                        'Ungültige ASM Opcode: %s',
                        '#uk=',
                        '#ru=');
ER_EXPECT_W_F  := trans('Expected "w" or "f".'           ,'Se esperaba "w" or "f".'         ,'',
                        '"w" oder "f" erwartet.',
                        '#uk=',
                        '#ru=');
ER_SYNTAX_ERR_ := trans('Syntax error: "%s"'             , 'Error de sintaxis: "%s"'        ,'',
                        'Syntax-Fehler: "%s"',
                        '#uk=',
                        '#ru=');
ER_DUPLIC_LBL_ := trans('Duplicated label: "%s"'         , 'Etiqueta duplicada: "%s"'       ,'',
                        'Dupliziertes Label: "%s"',
                        '#uk=',
                        '#ru=');
ER_EXPE_NUMBIT := trans('Expected number of bit: 0..7.'  , 'Se esperaba número de bit: 0..7','',
                        'Bitnummer erwarted: 0..7.',
                        '#uk=',
                        '#ru=');
ER_EXPECT_ADDR := trans('Expected address.'              , 'Se esperaba dirección'          ,'',
                        'Adresse erwarted.',
                        '#uk=',
                        '#ru=');
ER_EXPECT_BYTE := trans('Expected byte.'                 , 'Se esperaba byte'               ,'',
                        'Byte erwartet.',
                        '#uk=',
                        '#ru=');
ER_UNDEF_LABEL_:= trans('Undefined ASM Label: %s'        , 'Etiqueta ASM indefinida: %s'    ,'',
                        'Undefiniertes ASM Label:%s',
                        '#uk=',
                        '#ru=');
WA_ADDR_TRUNC  := trans('Address truncated to fit instruction.', 'Dirección truncada, al tamaño de la instrucción', '',
                        'Adresse abgeschnitten, um in Anweisung zu passen.',
                        '#uk=',
                        '#ru=');

