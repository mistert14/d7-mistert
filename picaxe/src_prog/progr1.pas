unit progr1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, epcWindows;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
  private
  abort:boolean;
  procedure run(cmd:string; txt:TStrings);


    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}



procedure _ExportEvent(Parent: Cardinal; const Output, Error: string; var AbortProcess: Boolean); stdcall;
var s:string;
begin
  if Parent > 0 then
  begin
    with TForm1(Parent) do
    begin
      if Output > '' then begin
        s:=memo1.Text;
        s:=s+Output;
        memo1.Text:=s;
        update;
      end;
      if Error > '' then
        memo1.Lines.Add(Error);

      AbortProcess := abort;
    end;
  end;
end;

procedure Tform1.run(cmd:string; txt:TStrings);
var
PipeIn : THandle;
PiPeOut : THandle;
Security : TSecurityAttributes;
StartupInfo : TStartupInfo;
ProcessInfo : TProcessInformation;
Buffer:array[0..4096] of Char;
NbRead:DWORD;
begin
 With Security do begin
   nlength := SizeOf(TSecurityAttributes) ;
   binherithandle := true;
   lpsecuritydescriptor := nil;
  end;
  CreatePipe(PipeIn,PiPeOut,@Security,0);
  FillChar(StartupInfo, SizeOf(TStartupInfo),0);
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  StartupInfo.wShowWindow := SW_HIDE;
  StartupInfo.hStdInput := PipeIn;
  StartupInfo.hStdOutput:= PiPeOut;
  StartupInfo.hStdError := PiPeOut;
  CreateProcess(nil,PChar(cmd),nil,nil,true,0,nil,nil,StartupInfo,ProcessInfo);
  WaitForSingleObject(ProcessInfo.hProcess,INFINITE);
  CloseHandle(PiPeOut);
  while ReadFile(PipeIn,Buffer,4096,NbRead,nil) do txt.Add(Buffer);
  CloseHandle(PipeIn);
end;
procedure TForm1.Button1Click(Sender: TObject);
begin
self.Memo1.Clear;
self.Button1.enabled:=false;
//self.run(edit1.Text,memo1.Lines);
self.Button1.enabled:=true;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
    ExitCode: Int64;
    OutPutText: string;
    ErrorText: string;

begin
 memo1.lines.Clear;
 self.abort:=false;
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

procedure TForm1.BtnCancelClick(Sender: TObject);
begin
self.abort:=true;
end;

end.
