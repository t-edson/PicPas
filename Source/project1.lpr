program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ConfigFrame, FormPrincipal, FormConfig, Parser, globales, ProcAsm,
  FormPICExplorer, FrameCfgIDE, XpresTypes, XpresElements, FormCodeExplorer;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.CreateForm(TConfig, Config);
  Application.CreateForm(TfrmPICExplorer, frmPICExplorer);
  Application.CreateForm(TfrmCodeExplorer, frmCodeExplorer);
//  Application.CreateForm(TfraCfgGeneral, fraCfgGeneral);
  Application.Run;
end.

