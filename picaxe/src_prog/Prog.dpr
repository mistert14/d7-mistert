program Prog;

uses
  Forms,
  progr1 in 'progr1.pas' {Form1},
  epcWindows in 'epcWindows.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
