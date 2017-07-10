Caption              := Trans('Settings'               , 'Configuración'            , '',
                              'Einstellungen');
BitAceptar.Caption   := Trans('&OK'                    , 'Aceptar'                  , '',
                              '&Ok');
BitAplicar.Caption   := Trans('&Apply'                 , 'Aplicar'                  , '',
                              '&Übernehmen');
BitCancel.Caption    := Trans('&Cancel'                , 'Cancelar'                  , '',
                              '&Abbrechen');

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
                              'Automatische Syntaxprüfung');

grpTabEdiState.Caption :=Trans('Tab Editor State'  , 'Estado de pestañas del editor', '',
                              'Registerkarte Editor Zustand');;
grpTabEdiState.Items[0]:=Trans('&Show always'      , 'Mostrar &Siempre'         , '',
                              '');
grpTabEdiState.Items[1]:=Trans('Hide for &One file', '&Ocultar si hay un archivo', '',
                              'Ausblenden für &eine Datei');
grpTabEdiState.Items[2]:=Trans('&Hide always'      , 'Ocultar &Siempre'          , '',
                              '&Immer ausblenden');

Label2.Caption       := Trans('Language'               , 'Lenguaje'                 , '',
                              'Sprache');

////////////////////////////////////////////////////////////////////////////
//////////////////////////  Environment Settings //////////////////////////
////////////////////////////////////////////////////////////////////////////
tabEnviron.Caption    := Trans('Environment', 'Entorno', '',
                               'Umgebung');
chkLoadLast.Caption   := Trans('Load last file edited'     , 'Cargar último archivo editado', '',
                               'Letzte editierte Datei laden');
lblMessPan1.Caption   := Trans('Message Panel Back color'  , 'Fondo del panel de mensajes', '',
                               'Nachrichtenpaneel Hintergrundfarbe');
lblMessPan2.Caption   := Trans('Message Panel Text color:' , 'Texto del panel de mensajes', '',
                               'Nachrichtenpaneel Textfarbe:');
lblMessPan3.Caption   := Trans('Message Panel Error color:', 'Error del panel de mensajes', '',
                               'Nachrichtenpaneel Fehlerfarbe:');
lblMessPan4.Caption   := Trans('Message Panel Selec. color:', 'Selec. del panel de mensajes', '',
                               'Nachrichtenpaneel Auswahlfarbe:');

lblPanelCol.Caption   := Trans('Panels Color:'             , 'Color de los paneles:', '',
                               'Paneelenfarbe:');
lblSplitterCol.Caption:= Trans('Splitters color:'          , 'Color de los separadores:', '',
                               'Trenner-Farbe:');
lblCodExplCol1.Caption:= Trans('Code Explorer Back color:' , 'Fondo del explor. de código:', '',
                               'Code-Explorer Hintergrundfarbe:');
lblCodExplCol2.Caption:= Trans('Code Explorer Text Color:' , 'Texto del explor. de código:', '',
                               'Code-Explorer Textfarbe:');
butDefval.Caption     := Trans('Set &Default Values'        , 'Valores por &Defecto', '',
                               'Setze &Standardwerte');

////////////////////////////////////////////////////////////////////////////
//////////////////////////// Editor Settings ////////////////////////////
////////////////////////////////////////////////////////////////////////////
tabEditor.Caption    := Trans('Editor'                 , 'Editor'                   , '',
                              'Editor');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// Syntax Settings ////////////////////////////
////////////////////////////////////////////////////////////////////////////
tabSyntax.Caption    := Trans('Syntax'                 , 'Sintaxis'                 , '',
                              'Syntax');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// Assembler settings ////////////////////////////
////////////////////////////////////////////////////////////////////////////
tabEnsamb.Caption    := Trans('Assembler'              , 'Ensamblador'              , '',
                              'Assembler');
chkIncHeadMpu.Caption:= Trans('Include MPU &Header'    , 'Incluir &Encabezado de MPU','',
                              'MPU &Kopfzeilen einbinden');
chkIncDecVar.Caption := Trans('Include &Variables declaration', 'Incluir Declaración de variables', '',
                              'Variablendeklaration einfügen');
RadioGroup2.Caption  := Trans('Style'                  , 'Estilo'                   , '',
                              'Stil');
chkExcUnused.Caption   := Trans('Exclude unused'         , 'Excluir no usadas'        , '',
                              'Unbenutzte ausschlieÃŸen');
chkIncAddress.Caption:= Trans('Include &Memory Address','Incluir &Dirección de memoria','',
                              'Speicheradressen einbinden');
chkIncComment.Caption:= Trans('Include &Comments'      , 'Incluir &Comentarios'     , '',
                              'Kommentare hinzufügen');
chkIncComment2.Caption:=Trans('Include &Detailed comments', 'Incluir Comentarios &detallados' , '',
                              '&Detaillierte Kommentare hinzufügen');
chkIncVarName.Caption:= Trans('Include &Variable Names','Incluir Nombre de &Variables','',
                              '&Variablennamen einbinden');
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
chkOptBnkAftIF.Caption := Trans('After IF structure'    , 'Después de instrucciones IF.', '',
                               'Nach IF-Struktur');
chkOptBnkBefPro.Caption:= Trans('Before Procedures'     , 'Antes de procedimientos.', '',
                               'Vor Prozeduren');
chkOptBnkAftPro.Caption:= Trans('After Procedures'      , 'Después de procedimientos.', '',
                                'Nach den Prozeduren');

