MSG_NOFIL := trans('No files'    , 'No hay archivos','','');
MSG_PASFI := trans('Pascal Files', 'Archivos Pascal','','');
MSG_ALLFI := trans('All files'   , 'Todos los archivos','','');
MSG_MODIF := trans('File %s has been modified. Save?',
                   'El archivo %s ha sido modificado. Guardar cambios?',
                   '','');
mnNewTab.Caption   := Trans('New', 'Nuevo','','');
mnCloseTab.Caption := Trans('Close', 'Cerrar','','');
if mnRecents<>nil then mnRecents.Caption:= trans('&Recents' ,'&Recientes','','');

