MSG_NOFILES  := trans('No files'    , 'No hay archivos','',
                      'Keine Dateien');
MSG_PASFILES := trans('Pascal Files', 'Archivos Pascal','',
                      'Pascal Dateien');
MSG_ALLFILES := trans('All files'   , 'Todos los archivos','',
                      'Alle Dateien');
MSG_MODIFSAV := trans('File %s has been modified. Save?',
                      'El archivo %s ha sido modificado. Guardar cambios?',
                      '',
                      'Die Datei %s wurde verändert. Speichern ?');
MSG_NOSYNFIL := trans('Syntax file not found: %s'   , 'Archivo de sintaxis no encontrado: %s','',
                      'Syntax-Datei nicht gefunden: %s');
lblBackground.Caption := Trans('<< No files >>', '<< Sin archivos >>', '', '<< Keine Dateien >>');
mnNewTab.Caption      := Trans('New', 'Nuevo','','Neu');
mnCloseTab.Caption    := Trans('Close', 'Cerrar','','Schließen');
mnCloseAll.Caption    := Trans('Close All', 'Cerrar todos','','Alle Schliessen');
if mnRecents<>nil then mnRecents.Caption:= trans('&Recents' ,'&Recientes','','&Letzte');

