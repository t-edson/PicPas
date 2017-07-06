//ASM blocks errors
ER_EXPEC_COMMA := trans('Expected ",".'                  , 'Se esperaba ","'                ,'',
                        '');
ER_EXP_ADR_VAR := trans('Expected address or variable name.','Se esperaba dirección o variable.','',
                        '');
ER_EXP_CON_VAL := trans('Expected constant or value.'    ,'Se esperaba constante o variable.','',
                        '');
ER_NOGETADD_VAR:= trans('Cannot get address of this Variable', 'No se puede obtener la dirección de esta variable.', '',
                        '');
ER_NOGETVAL_CON:= trans('Cannot get value of this costant', 'No se puede obtener el valor de esta constante.', '',
                        '');
ER_INV_ASMCODE := trans('Invalid ASM Opcode: %s'         , 'Instrucción inválida: %s'       ,'',
                        '');
ER_EXPECT_W_F  := trans('Expected "w" or "f".'           ,'Se esperaba "w" or "f".'         ,'',
                        '');
ER_SYNTAX_ERR_ := trans('Syntax error: "%s"'             , 'Error de sintaxis: "%s"'        ,'',
                        '');
ER_DUPLIC_LBL_ := trans('Duplicated label: "%s"'         , 'Etiqueta duplicada: "%s"'       ,'',
                        '');
ER_EXPE_NUMBIT := trans('Expected number of bit: 0..7.'  , 'Se esperaba número de bit: 0..7','',
                        '');
ER_EXPECT_ADDR := trans('Expected address.'              , 'Se esperaba dirección'          ,'',
                        '');
ER_EXPECT_BYTE := trans('Expected byte.'                 , 'Se esperaba byte'               ,'',
                        '');
ER_UNDEF_LABEL_:= trans('Undefined ASM Label: %s'        , 'Etiqueta ASM indefinida: %s'    ,'',
                        '');
WA_ADDR_TRUNC  := trans('Address truncated to fit instruction.', 'Dirección truncada, al tamaño de la instrucción', '',
                        '');

