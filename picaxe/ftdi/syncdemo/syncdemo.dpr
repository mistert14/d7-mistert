{ ----------------------------------------------------------------------------
  FTDIclass sync demo
  Copyright (c) Michael "Zipplet" Nixon 2009.
  Licensed under the MIT license, see license.txt in the project trunk.
  ---------------------------------------------------------------------------- }
program syncdemo;

uses
  Forms,
  unitfrmsyncdemo in 'unitfrmsyncdemo.pas' {frmsyncDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmsyncDemo, frmsyncDemo);
  Application.Run;
end.
