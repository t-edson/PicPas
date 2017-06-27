Caption              := Trans('Settings'               , 'Configuración'            , '',
                              'Einstellungen');
BitAceptar.Caption   := Trans('&OK'                    , 'Aceptar'                  , '',
                              '');
BitAplicar.Caption   := Trans('&Apply'                 , 'Aplicar'                  , '',
                              '');
BitCancel.Caption    := Trans('&Cancel'                , 'Cancelar'                  , '',
                              '');

////////////////////////////////////////////////////////////////////////////
//////////////////////////  General settings ///////////////////////////////
////////////////////////////////////////////////////////////////////////////
tabGeneral.Caption   := Trans('General'                 , 'General'                  , '',
                              'Allgemein');
RadioGroup1.Caption  := Trans('Toolbar'                 , 'Barra de herramientas'    , '',
                              'Werkzeugleiste');
RadioGroup1.Items[0] := Trans('Small Icons'             , 'Íconos pequeños'          , '',
                              'Kleine Bilder');
RadioGroup1.Items[1] := Trans('Big Icons'               , 'Íconos grandes'           , '',
                              'Große Bilder');
label1.Caption       := Trans('Units Path:'             , 'Ruta de unidades'         , '',
                              'Unitpfad:');
chkAutSynChk.Caption := Trans('Automatic Syntax checking', 'Verificac. Automática de sintaxis', '',
                              '');

grpTabEdiState.Caption :=Trans('Tab Editor State'  , 'Estado de pestañas del editor', '',
                              '');;
grpTabEdiState.Items[0]:=Trans('&Show always'      , 'Mostrar &Siempre'         , '',
                              '');
grpTabEdiState.Items[1]:=Trans('Hide for &One file', '&Ocultar si hay un archivo', '',
                              '');
grpTabEdiState.Items[2]:=Trans('&Hide always'      , 'Ocultar &Siempre'          , '',
                              '');

Label2.Caption       := Trans('Language'               , 'Lenguaje'                 , '',
                              'Sprache');

////////////////////////////////////////////////////////////////////////////
//////////////////////////  Environment Settings //////////////////////////
////////////////////////////////////////////////////////////////////////////
tabEnviron.Caption    := Trans('Environment', 'Entorno', '',
                               '');
chkLoadLast.Caption   := Trans('Load last file edited'     , 'Cargar último archivo editado', '',
                               'Letzte editierte Datei laden');
lblMessPan1.Caption   := Trans('Message Panel Back color'  , 'Fondo del panel de mensajes', '',
                               '');
lblMessPan2.Caption   := Trans('Message Panel Text color:' , 'Texto del panel de mensajes', '',
                               '');
lblMessPan3.Caption   := Trans('Message Panel Error color:', 'Error del panel de mensajes', '',
                               '');
lblMessPan4.Caption   := Trans('Message Panel Selec. color:', 'Selec. del panel de mensajes', '',
                               '');

lblPanelCol.Caption   := Trans('Panels Color:'             , 'Color de los paneles:', '',
                               '');
lblSplitterCol.Caption:= Trans('Splitters color:'          , 'Color de los separadores:', '',
                               '');
lblCodExplCol1.Caption:= Trans('Code Explorer Back color:' , 'Fondo del explor. de código:', '',
                               '');
lblCodExplCol2.Caption:= Trans('Code Explorer Text Color:' , 'Texto del explor. de código:', '',
                               '');
butDefval.Caption     := Trans('Set &Default Values'        , 'Valores por &Defecto', '',
                               '');

////////////////////////////////////////////////////////////////////////////
//////////////////////////// Editor Settings ////////////////////////////
////////////////////////////////////////////////////////////////////////////
tabEditor.Caption    := Trans('Editor'                 , 'Editor'                   , '',
                              'Editor');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// Syntax Settings ////////////////////////////
////////////////////////////////////////////////////////////////////////////
tabSyntax.Caption    := Trans('Syntax'                 , 'Sintaxis'                 , '',
                              '');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// Assembler settings ////////////////////////////
////////////////////////////////////////////////////////////////////////////
tabEnsamb.Caption    := Trans('Assembler'              , 'Ensamblador'              , '',
                              'Assembler');
chkIncHeadMpu.Caption:= Trans('Include MPU &Header'    , 'Incluir &Encabezado de MPU','',
                              'MPU &Kopfzeilen einfügen');
chkIncDecVar.Caption := Trans('Include &Variables declaration', 'Incluir Declaración de variables', '',
                              'Variablendeklaration einfügen');
RadioGroup2.Caption  := Trans('Style'                  , 'Estilo'                   , '',
                              'Stil');
chkExcUnused.Caption   := Trans('Exclude unused'         , 'Excluir no usadas'        , '',
                              'Unbenutzte ausschließen');
chkIncAddress.Caption:= Trans('Include &Memory Address','Incluir &Dirección de memoria','',
                              'Speicheradressen zufügen');
chkIncComment.Caption:= Trans('Include &Comments'      , 'Incluir &Comentarios'     , '',
                              'Kommentare hinzufügen');
chkIncComment2.Caption:=Trans('Include &Detailed comments', 'Incluir Comentarios &detallados' , '',
                              '&Detaillierte Kommentare hinzufügen');
chkIncVarName.Caption:= Trans('Include &Variable Names','Incluir Nombre de &Variables','',
                              '');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// Output Settings ///////////////////////////////
////////////////////////////////////////////////////////////////////////////
tabOutput.Caption    := Trans('Compiler'               , 'Compilador'               , '',
                              'Compiler');
chkShowErrMsg.Caption:= Trans('&Show Error Messages'   , '&Mostrar mensajes de error', '',
                              '&Zeige Fehlermeldungen');
grpOptimLev.Caption  := Trans('Optimization Level:'    , 'Nivel de optimización:'   , '',
                              'Optimierungsstufe:');
grpOptimLev.Items[0] := Trans('Fool'                   , 'Tonto'                    , '',
                              'Dumm');
grpOptimLev.Items[1] := Trans('Smart'                  , 'Inteligente'              , '',
                              'Schlau');
chkSetProIniBnk.Caption:= Trans('Set &Always Procedure &Initial Bank', 'Fijar siempre banco &Inicial en proc.', '',
                              '');
chkSetProEndBnk.Caption:= Trans('Set always Procedure &End Bank'     , 'Fijar siempre el banco &Final en proc.', '',
                              '');

