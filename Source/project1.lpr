program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, FormPrincipal, Parser, FormPICExplorer, GenCod, GenCodPic,
  XpresParserPIC, FrameSyntaxTree, XpresElementsPIC, Globales, FormConfig,
  PicPasProject, FrameEditView, FrameMessagesWin, FormElemProperty,
  FrameCfgSyntax, FrameCfgSynEdit, CodeTools, ParserAsm, ParserDirec,
  FrameCfgExtTool, FormDebugger, FrameRamExplorer, FormRAMExplorer,
  FrameRomExplorer, FramePicRegisters, FrameRegWatcher, FramePICDiagram;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.CreateForm(TfrmPICExplorer, frmPICExplorer);
  Application.CreateForm(TConfig, Config);
  Application.CreateForm(TfrmElemProperty, frmElemProperty);
  Application.CreateForm(TfrmDebugger, frmDebugger);
  Application.CreateForm(TfrmRAMExplorer, frmRAMExplorer);
//  Application.CreateForm(TfraCfgGeneral, fraCfgGeneral);
  Application.Run;
end.

