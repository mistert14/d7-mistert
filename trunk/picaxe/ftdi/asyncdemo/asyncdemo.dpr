{ ----------------------------------------------------------------------------
  FTDIclass async demo
  Copyright (c) Michael "Zipplet" Nixon 2009.
  Licensed under the MIT license, see license.txt in the project trunk.
  ---------------------------------------------------------------------------- }
program asyncdemo;

uses
  Forms,
  unitfrmasyncdemo in 'unitfrmasyncdemo.pas' {frmAsyncDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmAsyncDemo, frmAsyncDemo);
  Application.Run;
end.
