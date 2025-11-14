program extdl_gui;

{$MODE Delphi}
{$CODEPAGE UTF8}

uses
  Forms, Interfaces,
  ExtDlFeUnit in 'ExtDlFeUnit.pas' {ExtDlFe};

{$R *.res}

begin
			Application.Scaled:=True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TExtDlFe, ExtDlFe);
  Application.Run;
end.

