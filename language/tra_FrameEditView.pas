MSG_NOFILES  := trans('No files'    , 'No hay archivos','Mana khipukuna kanchu',
                      'Keine Dateien',
                        '#uk=',
                        '#ru=');
MSG_PASFILES := trans('Pascal Files', 'Archivos Pascal','Pascal khipu',
                      'Pascal Dateien',
                        '#uk=',
                        '#ru=');
MSG_ALLFILES := trans('All files'   , 'Todos los archivos','Llapan khipukuna',
                      'Alle Dateien',
                        '#uk=',
                        '#ru=');
MSG_MODIFSAV := trans('File %s has been modified. Save?',
                      'El archivo %s ha sido modificado. Guardar cambios?',
                      'Kay %s sutiyuq khipuqa huknirayakun. Waqaychankichu huknirayasqanta?',
                      'Die Datei %s wurde verändert. Speichern ?',
                        '#uk=',
                        '#ru=');
MSG_NOSYNFIL := trans('Syntax file not found: %s'   , 'Archivo de sintaxis no encontrado: %s','Kay kipup sintaxis nisqantaqa mana tarikunchu %s',
                      'Syntax-Datei nicht gefunden: %s',
                        '#uk=',
                        '#ru=');
lblBackground.Caption := Trans('<< No files >>', '<< Sin archivos >>', 'Mana khipukunayuq', '<< Keine Dateien >>',
                        '#uk=',
                        '#ru=');
mnNewTab.Caption      := Trans('New', 'Nuevo','Musuq','Neu',
                        '#uk=',
                        '#ru=');
mnCloseTab.Caption    := Trans('Close', 'Cerrar','Wisqay','Schließen',
                        '#uk=',
                        '#ru=');
mnCloseAll.Caption    := Trans('Close All', 'Cerrar todos','Llapanta wisqay','Alle Schliessen',
                        '#uk=',
                        '#ru=');
mnCloseOthers.Caption := Trans('Close &others', 'Cerrar &otros','','',
                        '#uk=',
                        '#ru=');
if mnRecents<>nil then mnRecents.Caption:= trans('&Recents' ,'&Recientes','','&Letzte',
                        '#uk=',
                        '#ru=');
