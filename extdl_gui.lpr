program extdl_gui;

{$MODE Delphi}
{$CODEPAGE UTF8}

uses
  Forms, Interfaces,
  ExtDlFeUnit in 'ExtDlFeUnit.pas' {ExtDlFe},
  uDarkStyleParams,
  uDarkStyleSchemes,
  uMetaDarkStyle
{ you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  PreferredAppMode := pamAllowDark;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
			Application.Scaled:=True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TExtDlFe, ExtDlFe);
  Application.Run;
end.

