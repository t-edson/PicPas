MSG_NOFILES  := trans('No files'    , 'No hay archivos','Mana khipukuna kanchu',
                      'Keine Dateien');
MSG_PASFILES := trans('Pascal Files', 'Archivos Pascal','Pascal khipu',
                      'Pascal Dateien');
MSG_ALLFILES := trans('All files'   , 'Todos los archivos','Llapan khipukuna',
                      'Alle Dateien');
MSG_MODIFSAV := trans('File %s has been modified. Save?',
                      'El archivo %s ha sido modificado. Guardar cambios?',
                      'Kay %s sutiyuq khipuqa huknirayakun. Waqaychankichu huknirayasqanta?',
                      'Die Datei %s wurde verändert. Speichern ?');
MSG_NOSYNFIL := trans('Syntax file not found: %s'   , 'Archivo de sintaxis no encontrado: %s','Kay kipup sintaxis nisqantaqa mana tarikunchu %s',
                      'Syntax-Datei nicht gefunden: %s');
lblBackground.Caption := Trans('<< No files >>', '<< Sin archivos >>', 'Mana khipukunayuq', '<< Keine Dateien >>');
mnNewTab.Caption      := Trans('New', 'Nuevo','Musuq','Neu');
mnCloseTab.Caption    := Trans('Close', 'Cerrar','Wisqay','Schließen');
mnCloseAll.Caption    := Trans('Close All', 'Cerrar todos','Llapanta wisqay','Alle Schliessen');
mnCloseOthers.Caption := Trans('Close &others', 'Cerrar &otros','','');
if mnRecents<>nil then mnRecents.Caption:= trans('&Recents' ,'&Recientes','','&Letzte');
