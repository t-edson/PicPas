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
                              'Einstellungen',
                              '#uk=',
                              '#ru=');
BitAceptar.Caption   := Trans('&OK'                    , 'Aceptar'                  , '',
                              '&Ok',
                              '#uk=',
                              '#ru=');
BitAplicar.Caption   := Trans('&Apply'                 , 'Aplicar'                  , '',
                              '&Übernehmen',
                              '#uk=',
                              '#ru=');
BitCancel.Caption    := Trans('&Cancel'                , 'Cancelar'                  , '',
                              '&Abbrechen',
                              '#uk=',
                              '#ru=');

////////////////////////////////////////////////////////////////////////////
//////////////////////////  Environment Settings //////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_ENVIRON     := Trans('Environment', 'Entorno', '',
                             'Umgebung',
                              '#uk=',
                              '#ru=');

Label2.Caption      := Trans('Language'               , 'Lenguaje'                 , '',
                              'Sprache',
                              '#uk=',
                              '#ru=');
RadioGroup1.Caption := Trans('Toolbar'                 , 'Barra de herramientas'    , '',
                              'Werkzeugleiste',
                              '#uk=',
                              '#ru=');
RadioGroup1.Items[0]:= Trans('Small Icons'             , 'Íconos pequeños'          , '',
                              'Kleine Bilder',
                              '#uk=',
                              '#ru=');
RadioGroup1.Items[1]:= Trans('Big Icons'               , 'Íconos grandes'           , '',
                              'Große Bilder',
                              '#uk=',
                              '#ru=');
label1.Caption      := Trans('Units Path:'             , 'Ruta de unidades'         , '',
                              'Unitpfad:',
                              '#uk=',
                              '#ru=');

label3.Caption      := Trans('&Set Theme'        , '&Fijar Tema', '',
                               '',
                              '#uk=',
                              '#ru=');
LABEL_THEM_NONE     := Trans('None', 'Ninguno', '',
                             '',
                              '#uk=',
                              '#ru=');
label4.Caption      := Trans('&Create Theme'        , '&Crear Tema', '',
                               '',
                              '#uk=',
                              '#ru=');
butSaveCurThem.Caption := Trans('&Save current config.', 'Guardar config. actual', '',
                             '',
                              '#uk=',
                              '#ru=');;

chkLoadLast.Caption := Trans('Load last file edited'     , 'Cargar último archivo editado', '',
                             'Letzte editierte Datei laden',
                              '#uk=',
                              '#ru=');

lblPanelCol.Caption := Trans('Panels Color:'             , 'Color de los paneles:', '',
                               'Paneelenfarbe:',
                              '#uk=',
                              '#ru=');
lblSplitCol.Caption := Trans('Splitters color:'          , 'Color de los separadores:', '',
                               'Trenner-Farbe:',
                              '#uk=',
                              '#ru=');

////////////////////////////////////////////////////////////////////////////
//////////////////////////  Code Explorer //////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_CODEXP    := Trans('Code Explorer', 'Explorador de Código', '',
                           '',
                              '#uk=',
                              '#ru=');
lblCodExplCol1.Caption:= Trans('Back color:' , 'Color de Fondo:', '',
                               'Hintergrundfarbe:',
                              '#uk=',
                              '#ru=');
lblCodExplCol2.Caption:= Trans('Text Color:' , 'Color de Texto:', '',
                               'Textfarbe:',
                              '#uk=',
                              '#ru=');
grpFilType.Caption    := Trans('File types shown:' , 'Tipos de archivos mostrados:', '',
                               '',
                              '#uk=',
                              '#ru=');
////////////////////////////////////////////////////////////////////////////
//////////////////////////  Message Panel //////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_MESPAN    := Trans('Message Panel', 'Panel de Mensajes', '',
                           '',
                              '#uk=',
                              '#ru=');
lblMessPan1.Caption   := Trans('Back color'   , 'Color de Fondo', '',
                               'Hintergrundfarbe',
                              '#uk=',
                              '#ru=');
lblMessPan2.Caption   := Trans('Text color:'  , 'Color de Texto', '',
                               'Textfarbe:',
                              '#uk=',
                              '#ru=');
lblMessPan3.Caption   := Trans('Error color:' , 'Color de Error', '',
                               'Fehlerfarbe:',
                              '#uk=',
                              '#ru=');
lblMessPan4.Caption   := Trans('Selection color:', 'Color de Selección', '',
                               'Auswahlfarbe:',
                              '#uk=',
                              '#ru=');

////////////////////////////////////////////////////////////////////////////
//////////////////////////  Editor settings ///////////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_EDITOR    := Trans('Editor'                 , 'Editor'                   , '',
                              'Editor',
                              '#uk=',
                              '#ru=');

Label6.Caption       := trans('&Font:'                 , '&Letra:'                     ,'',
                              'Schri&ftart:',
                              '#uk=',
                              '#ru=');
Label7.Caption       := trans('&Size:'                 , '&Tamaño:'                    ,'',
                              '&Größe:',
                              '#uk=',
                              '#ru=');
chkViewVScroll.Caption:= trans('&Vertical Scrollbar'    , 'Barra de desplaz &Vert.'     ,'',
                              '& Vertikale Bildlaufleiste',
                              '#uk=',
                              '#ru=');
chkViewHScroll.Caption:= trans('&Horizontal Scrollbar'  , 'Barra de desplaz &Horiz.'    ,'',
                              '&Horizontale Bildlaufleiste',
                              '#uk=',
                              '#ru=');

grpTabEdiState.Caption :=Trans('Tab Editor State'  , 'Estado de pestañas del editor', '',
                              'Registerkarte Editor Zustand',
                              '#uk=',
                              '#ru=');;
grpTabEdiState.Items[0]:=Trans('&Show always'      , 'Mostrar &Siempre'         , '',
                              '',
                              '#uk=',
                              '#ru=');
grpTabEdiState.Items[1]:=Trans('Hide for &One file', '&Ocultar si hay un archivo', '',
                              'Ausblenden für &eine Datei',
                              '#uk=',
                              '#ru=');
grpTabEdiState.Items[2]:=Trans('&Hide always'      , 'Ocultar &Siempre'          , '',
                              '&Immer ausblenden',
                              '#uk=',
                              '#ru=');

chkAutSynChk.Caption := Trans('Automatic Syntax checking', 'Verificac. Automática de sintaxis', '',
                              'Automatische Syntaxprüfung',
                              '#uk=',
                              '#ru=');

////////////////////////////////////////////////////////////////////////////
//////////////////////////// Editor Colors Settings ////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_EDICOL      := Trans('Colors'                 , 'Colores'                   , '',
                             '',
                              '#uk=',
                              '#ru=');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// Editor-Syntax Settings ////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_SYNTAX    := Trans('Syntax'                 , 'Sintaxis'                 , '',
                              'Syntax',
                              '#uk=',
                              '#ru=');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// Assembler settings ////////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_ASSEMB    := Trans('Assembler'              , 'Ensamblador'              , '',
                              'Assembler',
                              '#uk=',
                              '#ru=');
chkIncHeadMpu.Caption:= Trans('Include MPU &Header'    , 'Incluir &Encabezado de MPU','',
                              'MPU &Kopfzeilen einbinden',
                              '#uk=',
                              '#ru=');
chkIncDecVar.Caption := Trans('Include &Variables declaration', 'Incluir Declaración de variables', '',
                              'Variablendeklaration einfügen',
                              '#uk=',
                              '#ru=');
RadioGroup2.Caption  := Trans('Style'                  , 'Estilo'                   , '',
                              'Stil',
                              '#uk=',
                              '#ru=');
chkExcUnused.Caption   := Trans('Exclude unused'         , 'Excluir no usadas'        , '',
                              'Unbenutzte ausschlieÃŸen',
                              '#uk=',
                              '#ru=');
chkIncAddress.Caption:= Trans('Include &Memory Address','Incluir &Dirección de memoria','',
                              'Speicheradressen einbinden',
                              '#uk=',
                              '#ru=');
chkIncComment.Caption:= Trans('Include &Comments'      , 'Incluir &Comentarios'     , '',
                              'Kommentare hinzufügen',
                              '#uk=',
                              '#ru=');
chkIncComment2.Caption:=Trans('Include &Detailed comments', 'Incluir Comentarios &detallados' , '',
                              '&Detaillierte Kommentare hinzufügen',
                              '#uk=',
                              '#ru=');
chkIncVarName.Caption:= Trans('Include &Variable Names','Incluir Nombre de &Variables','',
                              '&Variablennamen einbinden',
                              '#uk=',
                              '#ru=');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// Output Settings ///////////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_COMPIL    := Trans('Compiler'               , 'Compilador'               , '',
                              'Compiler',
                              '#uk=',
                              '#ru=');
chkShowErrMsg.Caption:= Trans('&Show Error Messages'   , '&Mostrar mensajes de error', '',
                              '&Zeige Fehlermeldungen',
                              '#uk=',
                              '#ru=');
grpOptimLev.Caption  := Trans('Optimization Level:'    , 'Nivel de optimización:'   , '',
                              'Optimierungsstufe:',
                              '#uk=',
                              '#ru=');
grpOptimLev.Items[0] := Trans('Fool'                   , 'Tonto'                    , '',
                              'Dumm',
                              '#uk=',
                              '#ru=');
grpOptimLev.Items[1] := Trans('Smart'                  , 'Inteligente'              , '',
                              'Schlau',
                              '#uk=',
                              '#ru=');
chkOptBnkAftIF.Caption :=Trans('After IF structure'    , 'Después de instrucciones IF.', '',
                              'Nach IF-Struktur',
                              '#uk=',
                              '#ru=');
chkOptBnkBefPro.Caption:=Trans('Before Procedures'     , 'Antes de procedimientos.', '',
                              'Vor Prozeduren',
                              '#uk=',
                              '#ru=');
chkOptBnkAftPro.Caption:=Trans('After Procedures'      , 'Después de procedimientos.', '',
                               'Nach den Prozeduren',
                              '#uk=',
                              '#ru=');
chkReuProcVar.Caption := Trans('Reuse Procedures Variables', 'Reutilizar variables de proced.', '',
                               '',
                              '#uk=',
                              '#ru=');
////////////////////////////////////////////////////////////////////////////
//////////////////////////// External Tool ////////////////////////////
////////////////////////////////////////////////////////////////////////////
TIT_CFG_EXTOOL       := Trans('External Tool'           , 'Herramienta Externa'      , '',
                              '',
                              '#uk=',
                              '#ru=');
FillTree;
end;
