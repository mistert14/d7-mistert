program SimulAutov2;

uses
  Forms,
  Unit1v2 in 'Unit1v2.pas' {Form1},
  Unit2v2 in 'Unit2v2.pas' {Form2},
  Unit3v2 in 'Unit3v2.pas' {Form3},
  Unit4v2 in 'Unit4v2.pas' {Form4},
  progr1v2 in 'progr1v2.pas' {frmProg};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TfrmProg, frmProg);
  Application.Run;
end.
