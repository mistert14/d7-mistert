unit ConsoleUnit;
{30/4/00
This unit demonstrates a GUI application spawning a
console application, and capturing the output of the
console application to display in the GUI application.

Any matters arising, questions, comments etc... contact

Martin Lafferty
martinl@prel.co.uk

Production Robots Engineering Ltd
Box 2290, Wimborne, Dorset, BH21 2YY, England.

Background
----------
This example is based on a similar thing I wrote some years
ago which worked not very well under Win95 and not at all under
Windows NT. If you are one of the many people who wrote to me
asking me about this, I am sorry it has taken me so long to sort
it out. I didn't have a need for it until now, and I have been
busy - you know how it is.

The Win32 SDK has a topic called

"Creating a Child process with redirected input and output". I tried to
use that as a basis for this work but found it very confusing and could
not really get it to do what I wanted. The code presented here is really
based on information from Richter ("Advanced Windows" ISBN 1-57231-548-2)
notably chapters 2 (Kernel Objects) and chapter 3 (Processes)

Here is an interesting thing that might be bug (but I don't think so)

Try this on NT:
Open TestApp.dpr (simple console app, supplied) and compile
Open ConsoleTest.dpr in the Delphi IDE
Enter TestApp as command line.
You should get an output - testapp should return 0.

Now without closing down Delphi close ConsoleTest.dpr and reopen TestApp.dpr.
Try to compile and you will get a 'Cannot create output file' error - which
normally indicates that the EXE image is still loaded, but if you check the
process list using the NT Task manager there is no sign of Testapp.exe.

If you close Delphi, and restart it, you can compile OK.


It would be reasonable to assume that a bug in ConsoleTest.dpr was failing to
allow TestApp to terminate properly. I have looked for such a bug, and cannot
find anything. If you run ConsoleTest direct from NT (not in the IDE) then the
problem is not present. You can compile TestApp.dpr quite happily in the IDE
after running the EXE via ConsoleTest running outside the IDE. I am not too
sure what is going on here but it seems to be only a problem when TestApp is
running as a grandchild of Delphi. If you find out more, let me know.
}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TConsoleForm = class(TForm)
    Output: TListBox;
    Panel1: TPanel;
    CmdLineLabel: TLabel;
    CmdLineEdit: TEdit;
    BrowseButton: TButton;
    RunButton: TButton;
    BrowseDlg: TOpenDialog;
    procedure RunButtonClick(Sender: TObject);
    procedure BrowseButtonClick(Sender: TObject);
  private
    procedure RunningUpdate(Sender: TObject);
  public
    { Public declarations }
  end;

var
  ConsoleForm: TConsoleForm;

implementation

{$IFDEF DEBUG}
var
  ReadCount: Integer;
{$ENDIF}

function ExecConsoleApp(CommandLine: String;
                        AppOutput: TStrings;     {will receive output of child process}
                        OnNewLine: TNotifyEvent  {if assigned called on each new line}
                        ): Cardinal;

{child process has no input. I have not thought about this.
Function returns exit code of child process (normally 0 for no error)

If the function returns STILL_ACTIVE ($00000103) then the ReadLoop
has terminated before the app has finished executing. See comments in body
of function
}

const
  CR = #$0D;
  LF = #$0A;
  TerminationWaitTime = 5000;

var
  StartupInfo:TStartupInfo;
  ProcessInfo:TProcessInformation;
  SecurityAttributes: TSecurityAttributes;

  TempHandle,
  WriteHandle,
  ReadHandle: THandle;
  ReadBuf: array[0..$100] of Char;
  BytesRead: Cardinal;
  LineBuf: array[0..$100] of Char;
  LineBufPtr: Integer;
  Newline: Boolean;
  i: Integer;

procedure OutputLine;
begin
  LineBuf[LineBufPtr]:= #0;
  with AppOutput do
  if Newline then
    Add(LineBuf)
  else
    Strings[Count-1]:= LineBuf; {should never happen with count = 0}
  Newline:= false;
  LineBufPtr:= 0;
  if Assigned(OnNewLine) then
    OnNewLine(AppOutput)  {there is no reasonable justification for passing
                           AppOutput as self, but I don't have anything else}
end;

begin
  FillChar(StartupInfo,SizeOf(StartupInfo), 0);
  FillChar(ReadBuf, SizeOf(ReadBuf), 0);
  FillChar(SecurityAttributes, SizeOf(SecurityAttributes), 0);
{$IFDEF DEBUG}
  ReadCount:= 0;
{$ENDIF}
  LineBufPtr:= 0;
  Newline:= true;
  with SecurityAttributes do
  begin
    nLength:= Sizeof(SecurityAttributes);
    bInheritHandle:= true
  end;
  if not CreatePipe(ReadHandle, WriteHandle, @SecurityAttributes, 0) then
    RaiseLastWin32Error;
  {create a pipe to act as StdOut for the child. The write end will need
   to be inherited by the child process}

  try
    {Read end should not be inherited by child process}
    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      if not SetHandleInformation(ReadHandle, HANDLE_FLAG_INHERIT, 0) then
        RaiseLastWin32Error
    end else
    begin
      {SetHandleInformation does not work under Window95, so we
      have to make a copy then close the original}
      if not DuplicateHandle(GetCurrentProcess, ReadHandle,
        GetCurrentProcess, @TempHandle, 0, True, DUPLICATE_SAME_ACCESS) then
        RaiseLastWin32Error;
      CloseHandle(ReadHandle);
      ReadHandle:= TempHandle
    end;

    with StartupInfo do
    begin
      cb:= SizeOf(StartupInfo);
      dwFlags:= STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      wShowWindow:= SW_HIDE;
      hStdOutput:= WriteHandle
    end;
    {Initialise the startup info. I suspect that it is only safe to pass
    WriteHandle as hStdOutput because we are going to make sure that the
    child inherits it. This is not documented anywhere, but I am reasonably
    sure it is correct. We should not have to use STARTF_USESHOWWINDOW and
    wShowWindow:= SW_HIDE as we are going to tell CreateProcess not to
    bother with an output window, but it would appear that Windows 95
    ignores the CREATE_NO_WINDOW flag. Fair enough - it is not in the SDK
    documentation (I got it out of Richter). CREATE_NO_WINDOW definately works
    under NT 4.0, so it is worth doing}

    if not CreateProcess(nil, PChar(CommandLine), nil, nil,
       true,                   {inherit kernel object handles from parent}
       NORMAL_PRIORITY_CLASS or CREATE_NO_WINDOW,
                               {DETACHED_PROCESS relevant for Console parent only
                               No need to create an output window - it would be
                               blank anyway}
       nil,
       nil,
       StartupInfo,
       ProcessInfo) then
     RaiseLastWin32Error;

    CloseHandle(ProcessInfo.hThread);
    {not interested in threadhandle - close it}

    CloseHandle(WriteHandle);
    {close our copy of Write handle - Child has its own copy now. It is important
    to close ours, otherwise ReadFile may not return when child closes its
    StdOutput - this is the mechanism by which the following loop detects the
    termination of the child process: it does not poll GetExitCodeProcess.

    The clue to this behaviour is in the 'Anonymous Pipes' topic of Win32.hlp - quote

    "To read from the pipe, a process uses the read handle in a call to the
    ReadFile function. When a write operation of any number of bytes completes,
    the ReadFile call returns. The ReadFile call also returns when all handles
    to the write end of the pipe have been closed or if any errors occur before
    the read operation completes normally."

    On this basis (and going somewhat beyond that stated above) I have assumed that
    ReadFile will return TRUE when a write is completed at the other end of the pipe
    and will return FALSE when the write handle is closed at the other end.

    I have also assumed that ReadFile will return when its output buffer is full
    regardless of the size of the write at the other end.

    I have tested all these assumptions as best I can (under NT 4)}

    try
      while ReadFile(ReadHandle, ReadBuf, SizeOf(ReadBuf), BytesRead, nil) do
      begin
        {There are much more efficient ways of doing this: we don't really
        need two buffers, but we do need to scan for CR & LF &&&}
{$IFDEF Debug}
        Inc(ReadCount);
{$ENDIF}
        for  i:= 0 to BytesRead - 1 do
        begin
          if (ReadBuf[i] = LF) then
          begin
            Newline:= true
          end else
          if (ReadBuf[i] = CR) then
          begin
            OutputLine
          end else
          begin
            LineBuf[LineBufPtr]:= ReadBuf[i];
            Inc(LineBufPtr);
            if LineBufPtr >= (SizeOf(LineBuf) - 1) then {line too long - force a break}
            begin
              Newline:= true;
              OutputLine
            end
          end
        end
      end;
      WaitForSingleObject(ProcessInfo.hProcess, TerminationWaitTime);
      {The child process may have closed its stdoutput handle but not yet
      terminated, so will wait for up to five seconds to it a chance to
      terminate. If it has not done so after this time, then we will end
      up returning STILL_ACTIVE ($103)

      If you don't care about the exit code of the process, then you don't
      need this wait: having said that, unless the child process has a
      particularly longwinded cleanup routine, the wait will be very short
      in any event.
      I recommend you leave this wait in unless you have an intimate
      understanding of the child process you are spawining and are sure you
      don't want to wait for it}

      GetExitCodeProcess(ProcessInfo.hProcess, Result);
      OutputLine {flush the line buffer}
    finally
      CloseHandle(ProcessInfo.hProcess)
    end
  finally
    CloseHandle(ReadHandle);
  end
end;

{$R *.DFM}

procedure TConsoleForm.RunButtonClick(Sender: TObject);
var
  s: String;
  CAExitCode: Integer;
begin
  s:= CmdLineEdit.Text;
  Output.Items.Clear;
  Output.Items.Add('Executing ' + s);
  CAExitCode:= ExecConsoleApp(s, Output.Items, RunningUpdate);
{$IFDEF DEBUG}
  Output.Items.Add(Format('%s returned %d (rc = %d)', [s, CAExitCode, ReadCount]))
{$ELSE}
  Output.Items.Add(Format('%s returned %d', [s, CAExitCode]))
{$ENDIF}
end;

procedure TConsoleForm.RunningUpdate(Sender: TObject);
begin
  Output.Update  {flush paint messages to show progress}
end;

procedure TConsoleForm.BrowseButtonClick(Sender: TObject);
begin
  if BrowseDlg.Execute then
    CmdLineEdit.Text:= BrowseDlg.Filename
end;

end.
