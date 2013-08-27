program CreateProcess1;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFNDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  main in 'main.pas' {Form1};

{//$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
