{ ----------------------------------------------------------------------------
  FTDIclass enumeration demo
  Copyright (c) Michael "Zipplet" Nixon 2009.
  Licensed under the MIT license, see license.txt in the project trunk.
  ---------------------------------------------------------------------------- }
program enumdemo;

uses
  Forms,
  unitfrmenumdemo in 'unitfrmenumdemo.pas' {frmenumdemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tunitfrmenumdemo, frmenumdemo);
  Application.Run;
end.
