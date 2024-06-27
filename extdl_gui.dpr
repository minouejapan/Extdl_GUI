program extdl_gui;

{$TYPEINFO OFF}

uses
  Vcl.Forms,
  ExtDlFeUnit in 'ExtDlFeUnit.pas' {ExtDlFe};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TExtDlFe, ExtDlFe);
  Application.Run;
end.

