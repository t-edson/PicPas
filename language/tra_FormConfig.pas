var
  TIT_CFG_ENVIRON, TIT_CFG_MESPAN, TIT_CFG_CODEXP,
  TIT_CFG_EDITOR, TIT_CFG_SYNTAX,
  TIT_CFG_ASSEMB,  TIT_CFG_COMPIL, TIT_CFG_EXTOOL: String;
  LABEL_THEM_NONE, TIT_CFG_EDICOL: String;

procedure TConfig.SetLanguage;
begin
  fraCfgSynEdit.SetLanguage;
  fraCfgExtTool.SetLanguage;
  fraCfgSyntax.SetLanguage;

Caption              := Trans('Settings'               , 'Configuración'            , '',
                              'Einstellungen');
BitAceptar.Caption   := Trans('&OK'                    , 'Aceptar'                  , '',
                              '&Ok');
BitAplicar.Caption   := Trans('&Apply'                 , 'Aplicar'                  , '',
                              '&Übernehmen');
BitCancel.Caption    := Trans('&Cancel'                , 'Cancelar'                  , '',
                              '&Abbrechen');

////////////////////////////////////////////////////////////////////////////
//////////////////////////  Environment Settings //////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_ENVIRON     := Trans('Environment', 'Entorno', '',
                             'Umgebung');

Label2.Caption      := Trans('Language'               , 'Lenguaje'                 , '',
                              'Sprache');
RadioGroup1.Caption := Trans('Toolbar'                 , 'Barra de herramientas'    , '',
                              'Werkzeugleiste');
RadioGroup1.Items[0]:= Trans('Small Icons'             , 'Íconos pequeños'          , '',
                              'Kleine Bilder');
RadioGroup1.Items[1]:= Trans('Big Icons'               , 'Íconos grandes'           , '',
                              'Große Bilder');
label1.Caption      := Trans('Units Path:'             , 'Ruta de unidades'         , '',
                              'Unitpfad:');

label3.Caption      := Trans('&Set Theme'        , '&Fijar Tema', '',
                               '');
LABEL_THEM_NONE     := Trans('None', 'Ninguno', '',
                             '');
label4.Caption      := Trans('&Create Theme'        , '&Crear Tema', '',
                               '');
butSaveCurThem.Caption := Trans('&Save current config.', 'Guardar config. actual', '',
                             '');;

chkLoadLast.Caption := Trans('Load last file edited'     , 'Cargar último archivo editado', '',
                             'Letzte editierte Datei laden');

lblPanelCol.Caption := Trans('Panels Color:'             , 'Color de los paneles:', '',
                               'Paneelenfarbe:');
lblSplitCol.Caption := Trans('Splitters color:'          , 'Color de los separadores:', '',
                               'Trenner-Farbe:');

////////////////////////////////////////////////////////////////////////////
//////////////////////////  Code Explorer //////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_CODEXP    := Trans('Code Explorer', 'Explorador de Código', '',
                           '');
lblCodExplCol1.Caption:= Trans('Back color:' , 'Color de Fondo:', '',
                               'Hintergrundfarbe:');
lblCodExplCol2.Caption:= Trans('Text Color:' , 'Color de Texto:', '',
                               'Textfarbe:');
grpFilType.Caption    := Trans('File types shown:' , 'Tipos de archivos mostrados:', '',
                               '');
////////////////////////////////////////////////////////////////////////////
//////////////////////////  Message Panel //////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_MESPAN    := Trans('Message Panel', 'Panel de Mensajes', '',
                           '');
lblMessPan1.Caption   := Trans('Back color'   , 'Color de Fondo', '',
                               'Hintergrundfarbe');
lblMessPan2.Caption   := Trans('Text color:'  , 'Color de Texto', '',
                               'Textfarbe:');
lblMessPan3.Caption   := Trans('Error color:' , 'Color de Error', '',
                               'Fehlerfarbe:');
lblMessPan4.Caption   := Trans('Selection color:', 'Color de Selección', '',
                               'Auswahlfarbe:');

////////////////////////////////////////////////////////////////////////////
//////////////////////////  Editor settings ///////////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_EDITOR    := Trans('Editor'                 , 'Editor'                   , '',
                              'Editor');

Label6.Caption       := trans('&Font:'                 , '&Letra:'                     ,'',
                              'Schri&ftart:');
Label7.Caption       := trans('&Size:'                 , '&Tamaño:'                    ,'',
                              '&Größe:');
chkViewVScroll.Caption:= trans('&Vertical Scrollbar'    , 'Barra de desplaz &Vert.'     ,'',
                              '& Vertikale Bildlaufleiste');
chkViewHScroll.Caption:= trans('&Horizontal Scrollbar'  , 'Barra de desplaz &Horiz.'    ,'',
                              '&Horizontale Bildlaufleiste');

grpTabEdiState.Caption :=Trans('Tab Editor State'  , 'Estado de pestañas del editor', '',
                              'Registerkarte Editor Zustand');;
grpTabEdiState.Items[0]:=Trans('&Show always'      , 'Mostrar &Siempre'         , '',
                              '');
grpTabEdiState.Items[1]:=Trans('Hide for &One file', '&Ocultar si hay un archivo', '',
                              'Ausblenden für &eine Datei');
grpTabEdiState.Items[2]:=Trans('&Hide always'      , 'Ocultar &Siempre'          , '',
                              '&Immer ausblenden');

chkAutSynChk.Caption := Trans('Automatic Syntax checking', 'Verificac. Automática de sintaxis', '',
                              'Automatische Syntaxprüfung');

////////////////////////////////////////////////////////////////////////////
//////////////////////////// Editor Colors Settings ////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_EDICOL    := Trans('Colors'                 , 'Colores'                   , '',
                           '');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// Editor-Syntax Settings ////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_SYNTAX    := Trans('Syntax'                 , 'Sintaxis'                 , '',
                              'Syntax');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// Assembler settings ////////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_ASSEMB    := Trans('Assembler'              , 'Ensamblador'              , '',
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
TIT_CFG_COMPIL    := Trans('Compiler'               , 'Compilador'               , '',
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
chkReuProcVar.Caption := Trans('Reuse Procedures Variables', 'Reutilizar variables de proced.', '',
                               '');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// External Tool ////////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_EXTOOL    := Trans('External Tool'           , 'Herramienta Externa'      , '',
                               '');
FillTree;
end;
