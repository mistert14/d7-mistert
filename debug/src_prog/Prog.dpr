program Prog;

uses
  Forms,
  progr1 in 'progr1.pas' {frmProg},
  epcWindows in 'epcWindows.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmProg, frmProg);
  Application.Run;
end.
