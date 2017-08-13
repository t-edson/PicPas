var
  ER_FAIL_EXEC_, PRE_TOOL_NAME: string;

procedure TfraCfgExtTool.SetLanguage(idLang: string);
begin
  curLang := idLang;
  //Editor settings
  label4.Caption      := trans('Tools:'           , 'Herramientas:'        ,'Llamk''anakuna:',
                               '');
  label1.Caption      := trans('Name:'            , 'Nombre:'              ,'Suti:',
                               '');
  label2.Caption      := trans('Program Path:'    , 'Ruta del programa:'   ,'Programapa ñannin:',
                               '');
  label3.Caption      := trans('Command line:'    , 'Línea de comando:'     ,'',
                               '');
  label5.Caption      := trans('To reference the output *.hex file, use $(hexFile)',
                               'Para referirse al archivo de salida, usar $(hexFile)', '',
                               '');
  chkWaitExit.Caption := trans('&Wait on exit'    , '&Esperar a terminar'   ,'Tukurinanta &Suyariy',
                              '');
  chkShowTBar.Caption := trans('&Show in Toolbar' , '&Mostrar en la barra de Herram.' ,'',
                              '');
  butTest.Caption     := trans('&Test'            , '&Probar'                ,'',
                               '');
  butAdd.Caption     := trans('&Add'              , '&Agregar'                ,'',
                               '');
  butRemove.Caption  := trans('&Remove'           , '&Eliminar'                ,'',
                               '');

  ER_FAIL_EXEC_   := trans('Fail executing: %s', 'Falla ejecutando: %s', '',
                           '');
  PRE_TOOL_NAME   := trans('Tool'              , 'Herramienta', '',
                           '');

end;
