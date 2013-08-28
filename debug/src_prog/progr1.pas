unit progr1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, epcWindows;

type
  TfrmProg = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private



    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  frmProg: TfrmProg;

implementation

{$R *.dfm}



procedure _ExportEvent(Parent: Cardinal; const Output, Error: string; var AbortProcess: Boolean); stdcall;
var s:string;
begin
  if Parent > 0 then
  begin
    with TfrmProg(Parent) do
    begin
      if Output > '' then begin
        s:=memo1.Text;
        s:=s+Output;
        memo1.Text:=s;
        update;
      end;
      if Error > '' then
        memo1.Lines.Add(Error);


    end;
  end;
end;


procedure TfrmProg.Button1Click(Sender: TObject);
begin
self.Memo1.Clear;
self.Button1.enabled:=false;
self.Button1.enabled:=true;
end;

procedure TfrmProg.Button2Click(Sender: TObject);
var
    ExitCode: Int64;
    OutPutText: string;
    ErrorText: string;

begin
 memo1.lines.Clear;
 self.Button2.Enabled:=false;
 epcWindows.CallCmd(
    ExtractFileDir(Application.ExeName),
    'picaxe18m2.exe',
    '-cCOM3 test.bas',
    ExitCode,
    OutPutText,
    ErrorText,
    10,
    Cardinal(Self),
    @_ExportEvent
   );
  self.Button2.Enabled:=true;
end;

end.
