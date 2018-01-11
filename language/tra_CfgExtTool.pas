var
  ER_FAIL_EXEC_, PRE_TOOL_NAME: string;

procedure TfraCfgExtTool.SetLanguage;
begin
  //Editor settings
  label4.Caption      := trans('Tools:'           , 'Herramientas:'        ,'Llamk''anakuna:',
                               '',
                              '#uk=',
                              '#ru=');
  label1.Caption      := trans('Name:'            , 'Nombre:'              ,'Suti:',
                               '',
                              '#uk=',
                              '#ru=');
  label2.Caption      := trans('Program Path:'    , 'Ruta del programa:'   ,'Programapa ñannin:',
                               '',
                              '#uk=',
                              '#ru=');
  label3.Caption      := trans('Command line:'    , 'Línea de comando:'     ,'',
                               '',
                              '#uk=',
                              '#ru=');
  label5.Caption      := trans('To reference the output *.hex file, use $(hexFile)',
                               'Para referirse al archivo de salida, usar $(hexFile)', '',
                               '',
                              '#uk=',
                              '#ru=');
  label6.Caption      := trans('To reference the source file, use ${mainFile}',
                               'Para referirse al archivo fuente, usar $(mainFile)', '',
                               '',
                              '#uk=',
                              '#ru=');
  chkWaitExit.Caption := trans('&Wait on exit'    , '&Esperar a terminar'   ,'Tukurinanta &Suyariy',
                              '',
                              '#uk=',
                              '#ru=');
  chkShowTBar.Caption := trans('&Show in Toolbar' , '&Mostrar en la barra de Herram.' ,'',
                              '',
                              '#uk=',
                              '#ru=');
  butTest.Caption     := trans('&Test'            , '&Probar'                ,'',
                               '',
                              '#uk=',
                              '#ru=');
  butAdd.Caption     := trans('&Add'              , '&Agregar'                ,'',
                               '',
                              '#uk=',
                              '#ru=');
  butRemove.Caption  := trans('&Remove'           , '&Eliminar'                ,'',
                               '',
                              '#uk=',
                              '#ru=');

  ER_FAIL_EXEC_      := trans('Fail executing: %s', 'Falla ejecutando: %s', '',
                              '',
                              '#uk=',
                              '#ru=');
  PRE_TOOL_NAME      := trans('Tool'              , 'Herramienta', '',
                              '',
                              '#uk=',
                              '#ru=');

end;
