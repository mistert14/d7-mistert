program ConsoleTest;

uses
  Forms,
  ConsoleUnit in 'ConsoleUnit.pas' {ConsoleForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TConsoleForm, ConsoleForm);
  Application.Run;
end.
