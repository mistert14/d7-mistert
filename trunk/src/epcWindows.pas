unit epcWindows;
 
interface
 
uses Windows, SysUtils;
 
{* -----------------------------------------------------------------------------
TCallCmdEvent pointeur de proc�dure ...
@param Parent identifiant priv� utilis� dans WaitEvent fourni par la fonction appelante de CallCmd
@param Output contiennent les derniers �l�ments envoy�s par le programme console sur le canal StdOut
@param Error contiennent les derniers �l�ments envoy�s par le programme console sur le canal StdError
@param AbortProcess indique si la processus doit �tre arr�t�
------------------------------------------------------------------------------ }
type
  TCallCmdEvent = procedure(Parent: Cardinal; const Output, Error: string; var AbortProcess: Boolean); stdcall;
 
function CallCmd(const CmdDirectory, CmdName, CmdParam: string; out ExitCode: Int64; out OutputText: string; out ErrorText: string; Delay: Cardinal = INFINITE; Parent: Cardinal = 0; WaitEvent: TCallCmdEvent = nil; PipeMaxSize: Cardinal = 0): Boolean;
 
implementation
 
uses Math;
 
{* -----------------------------------------------------------------------------
la fonction CallCmd permet de lancer un programme console, tout en r�cup�rant en quasi temps-r�el le contenu devant normalement s'y afficher
@param CmdDirectory Dossier contenant le Fichier CmdName
@param CmdName programme console � executer
@param CmdParam param�tres de la ligne de commande
@param ExitCode Code de Sortie renvoy� par le programme console, -1 si non r�cup�r�
@param OutputText chaine contenant tout ce qui aurait du s'afficher (canal sortie)
@param ErrorText chaine contenant tout ce qui a �t� signal� comme erreurs (canal erreur)
@param Delay indique le temps entre chaque cycle de lecture des canaux, d�termine la fr�quence de lancement de WaitEvent, par d�faut, cela attend que le programme console se termine
@param Parent identifiant priv� utilis� dans WaitEvent fourni par la fonction appelante de CallCmd
@param WaitEvent proc�dure � lancer lorsque le Delay est �coul�, Output et Error contiennent les derniers �l�ments envoy�s par le programme console sur les canaux depuis le dernier d�lai, AbortProcess indique si la processus doit �tre arr�t�
@param PipeMaxSize d�fini la taille maximal que l'on lit � chaque chaque cycle de lecture des canaux, si z�ro, taille non limit�e par d�faut
@return Indique si le programme a �t� lanc�
------------------------------------------------------------------------------ }
function CallCmd(const CmdDirectory, CmdName, CmdParam: string; out ExitCode: Int64; out OutputText: string; out ErrorText: string; Delay: Cardinal = INFINITE; Parent: Cardinal = 0; WaitEvent: TCallCmdEvent = nil; PipeMaxSize: Cardinal = 0): Boolean;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  CmdLine: string; // utile pour le d�bogage
  SecurityAttr : TSecurityAttributes;
  hReadPipeInput, hWritePipeInput: Cardinal;
  hReadPipeOutput, hWritePipeOutput: Cardinal;
  hReadPipeError, hWritePipeError: Cardinal;
  Terminated: Boolean;
  AbortProcess: Boolean;
  HandleFunctionProcess: Cardinal;
 
  function ReadPipe(Handle: Cardinal; out Buf: string): Boolean;
  const
    MAX_INT: Cardinal = MaxInt;
  var
    PipeSize: Cardinal;
    PipeToRead, PipeReaded: Cardinal;
  begin
    PipeSize := GetFileSize(Handle, nil); // On oublie si cela d�passe 2Go ... normalement c'est 4Ko
    if (PipeMaxSize > 0) and (PipeSize > PipeMaxSize) then
      PipeToRead := PipeMaxSize
    else
      PipeToRead := PipeSize;
 
    Result := PipeToRead > 0;
    if Result then
    begin
      SetLength(Buf, PipeToRead);
      ZeroMemory(@Buf[1], PipeToRead);
      ReadFile(Handle, Buf[1], PipeToRead, PipeReaded, nil);
    end;
  end;
 
  procedure ReadPipes();
  var
    DeltaOutputText: string;
    DeltaErrorText: string;
  begin
    if ReadPipe(hReadPipeOutput, DeltaOutputText) then
      OutputText := OutputText + DeltaOutputText;
    if ReadPipe(hReadPipeError, DeltaErrorText) then
      ErrorText := ErrorText + DeltaErrorText;
    try
      if Assigned(WaitEvent) then
        WaitEvent(Parent, DeltaOutputText, DeltaErrorText, AbortProcess);
    except
      on E: Exception do
        OutputDebugString(PChar(Format('epcWindows.CallCmd.ReadPipes.WaitEvent - "%s" : "%s"', [E.ClassName, E.Message])));
    end;
  end;
 
begin
  (*
  Result := True;
  OutputText := 'Dummy Output';
  ErrorText := 'Dummy Error';
  ErrorCode := 0;
  Exit;
  *)
  OutputText := '';
  ErrorText := '';
  try
    SecurityAttr.nLength := SizeOf(TSecurityAttributes);
    SecurityAttr.lpSecurityDescriptor := nil;
    SecurityAttr.bInheritHandle := True;
    if CreatePipe(hReadPipeInput, hWritePipeInput, @SecurityAttr, 0) and
      CreatePipe(hReadPipeOutput, hWritePipeOutput, @SecurityAttr, 0) and
      CreatePipe(hReadPipeError, hWritePipeError, @SecurityAttr, 0) then
    begin
      try
        ZeroMemory(@StartupInfo, SizeOf(StartupInfo)); // GetStartupInfo(StartupInfo);
        StartupInfo.cb := SizeOf(StartupInfo);
        StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES; // Active wShowWindow et hStdOutput/hStdError
        StartupInfo.wShowWindow := SW_HIDE;
        StartupInfo.hStdInput := hReadPipeInput;
        StartupInfo.hStdOutput := hWritePipeOutput;
        StartupInfo.hStdError := hWritePipeError;
        ZeroMemory(@ProcessInfo, SizeOf(ProcessInfo));
        CmdLine := Format('"%s%s" %s', [IncludeTrailingPathDelimiter(CmdDirectory), CmdName, CmdParam]);
        Result := CreateProcess(nil, PChar(CmdLine), @SecurityAttr, @SecurityAttr, True, 0, nil, PChar(CmdDirectory), StartupInfo, ProcessInfo);
        if Result then
        begin
          try
            Terminated := False;
            AbortProcess := False;
            while not Terminated do
            begin
              case WaitForSingleObject(ProcessInfo.hProcess, Delay) of
                WAIT_OBJECT_0 :
                  begin
                    ReadPipes();
                    Terminated := True;
                  end;
                WAIT_ABANDONED : Terminated := True;
                WAIT_TIMEOUT :
                  begin
                    ReadPipes();
                    Terminated := Delay = INFINITE;
                  end;
                WAIT_FAILED: Abort;
              else
                Terminated := True;
              end;
 
              if AbortProcess then
              begin
               HandleFunctionProcess := OpenProcess(PROCESS_TERMINATE, False, ProcessInfo.dwProcessId);
               if HandleFunctionProcess > 0 then
               begin
                 TerminateProcess(HandleFunctionProcess, 0);
                 CloseHandle(HandleFunctionProcess);
               end;
              end;
            end;
 
            TULargeInteger(ExitCode).HighPart := 0;
            if not GetExitCodeProcess(ProcessInfo.hProcess, TULargeInteger(ExitCode).LowPart) then
              ExitCode := -1;
          finally
            CloseHandle(ProcessInfo.hThread);
            CloseHandle(ProcessInfo.hProcess); // The handles for both the process and the main thread must be closed through calls to CloseHandle
          end;
        end;
      finally
        CloseHandle(hReadPipeInput);
        CloseHandle(hWritePipeInput);
        CloseHandle(hReadPipeOutput);
        CloseHandle(hWritePipeOutput);
        CloseHandle(hReadPipeError);
        CloseHandle(hWritePipeError);
      end;
    end
    else
      raise Exception.Create('Impossible de cr�er les Pipes');
  except
    on E: Exception do
    begin
      OutputDebugString(PChar(Format('epcWindows.CallCmd Error %s, Message : %s', [E.ClassName, E.Message])));
      raise;
    end;
  end;
end;
 
 
end.
