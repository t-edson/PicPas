MSG_NOFIL := trans('No files'    , 'No hay archivos','','Keine Dateien');
MSG_PASFI := trans('Pascal Files', 'Archivos Pascal','','Pascal Dateien');
MSG_ALLFI := trans('All files'   , 'Todos los archivos','','Alle Dateien');
MSG_MODIF := trans('File %s has been modified. Save?',
                   'El archivo %s ha sido modificado. Guardar cambios?',
                   '',
                   'Die Datei %s wurde verändert. Speichern ?');
mnNewTab.Caption   := Trans('New', 'Nuevo','','Neu');
mnCloseTab.Caption := Trans('Close', 'Cerrar','','Schließen');
if mnRecents<>nil then mnRecents.Caption:= trans('&Recents' ,'&Recientes','','&Letzte');

