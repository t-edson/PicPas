program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FormPrincipal, Parser, ProcAsm, FormPICExplorer, FormCodeExplorer,
  GenCod, GenCodPic, XpresParserPIC, FrameSyntaxTree, XpresElementsPIC,
  Globales, FormConfig, PicPasProject, FrameEditView, FrameMessagesWin;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.CreateForm(TfrmPICExplorer, frmPICExplorer);
  Application.CreateForm(TfrmCodeExplorer, frmCodeExplorer);
  Application.CreateForm(TConfig, Config);
//  Application.CreateForm(TfraCfgGeneral, fraCfgGeneral);
  Application.Run;
end.

