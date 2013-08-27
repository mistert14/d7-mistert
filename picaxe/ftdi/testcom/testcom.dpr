program testcom;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  WbemScripting_TLB in 'C:\Program Files (x86)\Borland\Delphi7\Imports\WbemScripting_TLB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
