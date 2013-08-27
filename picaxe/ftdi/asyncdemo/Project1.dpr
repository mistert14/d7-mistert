program Project1;

uses
  Forms,
  unitfrmenumdemo in '..\enumdemo\unitfrmenumdemo.pas' {unitfrmenumdemo2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tfrmenumdemo, frmenumdemo);
  Application.Run;
end.
