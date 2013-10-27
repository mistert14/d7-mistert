program SimulAutoMini;

uses
  Forms,
  Unit1Mini in 'Unit1Mini.pas' {Form21},
  Unit2Mini in 'Unit2Mini.pas' {Form22},
  Unit3Mini in 'Unit3Mini.pas' {Form23},
  Unit4Mini in 'Unit4Mini.pas' {Form24},
  progr1Mini in 'progr1Mini.pas' {frmProg2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm21, Form21);
  Application.CreateForm(TForm22, Form22);
  Application.CreateForm(TForm23, Form23);
  Application.CreateForm(TForm24, Form24);
  Application.CreateForm(TfrmProg2, frmProg2);
  Application.Run;
end.
