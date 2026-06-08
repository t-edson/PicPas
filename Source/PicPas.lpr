program PicPas;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, FormPrincipal, FormPICExplorer, FrameSyntaxTree,
  XpresElementsPIC, Globales, FormConfig, PicPasProject, FrameEditView,
  FrameMessagesWin, FormElemProperty, ParserAsm_PIC16, FrameCfgExtTool,
  FormDebugger, FormRAMExplorer, ParserDirec, CompBase;

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

