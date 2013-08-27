unit main;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, FileUtil;

type
  TForm1 = class(TForm)
    Button1: TButton;
    BitBtn1: TBitBtn;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{---------------------CreateDOSProcessRedirected------------------
Description :
Execute une application console indiquée en redirigeant l'entrée standard
(STDIN / input ) vers le fichier InputFile et la sortie standard (STDOUT/ output )
vers le fichier OutputFile.

Resultat :
True si la fonction réussie

Paramètres :
           CommandLine : Ligne de commande de l'application, le path est au format
                        chemin complet. Ex : C:\tools\App.exe
           InputFile : Nom du fichier fichier ascci contenant les commandes à
                       injecter dans l'entrée standard.
           OutputFile : Nom du fichier fichier ascci contenant le résultat
                       (l'affichage) du programme exécuté.
           ErrMsg : Message d'erreur additionel, peut être vide.

Version Delphi : 2, 3, 4, 5
Author : Theodoros Bebekis, email bebekis@otenet.gr

Exemple d'appel :
CreateDOSProcessRedirected('C:\MyDOSApp.exe','C:\Temp\InPut.txt',
                           'C:\Temp\OutPut.txt','')
------------------------------------------------------------------}
Function CreateDOSProcessRedirected(const CommandLine, InputFile,
                                    OutputFile, ErrMsg :string):boolean;
const
 ROUTINE_ID = '[fonction: CreateDOSProcessRedirected ]';
var
 OldCursor : TCursor;
 pCommandLine : array[0..MAX_PATH] of char;
 pInputFile, pOutPutFile : array[0..MAX_PATH] of char;
 StartupInfo : TStartupInfo;
 ProcessInfo : TProcessInformation;
 SecAtrrs : TSecurityAttributes;
 hAppProcess,hAppThread,hInputFile,hOutputFile : THandle;
begin
 Result := False;
 { Contrôle l'existence de fichier d'entrée( InputFile ) }
 if not FileExistsUTF8(InputFile) { *Converted from FileExists*  }
 then raise Exception.CreateFmt(ROUTINE_ID + #10 + #10 +
                                'le fichier d''entrée * %s *' + #10 +
                                'n''existe pas' + #10 + #10 +
                                 ErrMsg, [InputFile]);
 { Mémorise le curseur }
 OldCursor := Screen.Cursor;
 Screen.Cursor := crHourglass;
 { conversion strings Pascal vers PChar (null terminated strings) }
 StrPCopy(pCommandLine, CommandLine);
 StrPCopy(pInputFile, InputFile);
 StrPCopy(pOutPutFile, OutputFile);
TRY
 { Prépare la structure SecAtrrs pour l'appel de CreateFile.
 La structure SecAtrrs est nécessaire dans ce cas car
 nous voulons retourner un handle qui peut être héritée
 par le process enfant. 

 Voir les restrictions pour Win95,98 et ME :
 http://msdn.microsoft.com/library/default.asp?url=/library/en-us/fileio/base/createfile.asp
 }

 FillChar(SecAtrrs, SizeOf(SecAtrrs), #0);
 SecAtrrs.nLength := SizeOf(SecAtrrs);
 SecAtrrs.lpSecurityDescriptor := nil;
 SecAtrrs.bInheritHandle := True;

 { Crée le handle approprié pour le fichier input file }
 hInputFile := FileCreate(pInputFile); { *Converted from CreateFile*  }

 { est-ce que hInputFile est un handle valide ? }
 if hInputFile = INVALID_HANDLE_VALUE
 then
  raise Exception.CreateFmt(ROUTINE_ID + #10 + #10 +
              'la fonction WinApi CreateFile a retournée une' +
              'valeur de handle invalide' + #10 +
              'pour le fichier d''entrée * %s *' + #10 + #10 +
              ErrMsg, [InputFile]);

 { Crée le handle approprié pour le fichier output file }
 hOutputFile := FileCreate(pOutPutFile); { *Converted from CreateFile*  }

 { est-ce que hOutputFile est un handle valide ? }
 if hOutputFile = INVALID_HANDLE_VALUE
 then
   raise Exception.CreateFmt(ROUTINE_ID + #10 + #10 +
              'la fonction WinApi CreateFile a retournée une' +
              'valeur de handle invalide' + #10 +
              'pour le fichier de sortie * %s *' + #10 + #10 +
              ErrMsg, [OutputFile]);

 { Prépare la structure StartupInfo }
 FillChar(StartupInfo, SizeOf(StartupInfo), #0);
 StartupInfo.cb := SizeOf(StartupInfo);
 StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
 StartupInfo.wShowWindow := SW_HIDE;
 StartupInfo.hStdOutput := hOutputFile;
 StartupInfo.hStdInput := hInputFile;

 { Crée l'application/process }
 Result := CreateProcess(nil, { pointeur sur le nom de l'exécutable  }
                         pCommandLine, { pointeur sur la ligne de commande}
                         nil, { pointeur sur les attributs de sécurité du process  }
                         nil, { pointeur sur les attributs de sécurité du thread  }
                         True, { flag d'héritage du handle }
                         HIGH_PRIORITY_CLASS, { flags de création }
                         nil, { pointeur sur un nouveau bloc d'environment }
                         nil, { pointeur sur le nom du répertoire courant }
                         StartupInfo, { pointeur sur STARTUPINFO }
                         ProcessInfo); { pointeur sur PROCESS_INF }

 { Attend la fin de l'application pour terminer le traitement et
  récupérer les handles pour une libération ultérieure }
 if Result
  then begin
   WaitforSingleObject(ProcessInfo.hProcess, INFINITE);
   hAppProcess := ProcessInfo.hProcess;
   hAppThread := ProcessInfo.hThread;
  end else
  raise Exception.Create(ROUTINE_ID + #10 + #10 +
                 'echec de la fonction' + #10 + #10 +
                 ErrMsg);
 FINALLY
 { Ferme les handles de fichier.
  Les objets Kernel, comme le process et les fichiers, sont fermés pour informer
  le système que nous n'avons plus besoin de ces objets.}
  if hOutputFile <> INVALID_HANDLE_VALUE then FileClose(hOutputFile); { *Converted from CloseHandle*  }
  if hInputFile <> INVALID_HANDLE_VALUE then FileClose(hInputFile); { *Converted from CloseHandle*  }
  if hAppThread <> INVALID_HANDLE_VALUE then FileClose(hAppThread); { *Converted from CloseHandle*  }
  if hAppProcess <> INVALID_HANDLE_VALUE then FileClose(hAppProcess); { *Converted from CloseHandle*  }

  { Restaure l'ancien curseur }
  Screen.Cursor:= OldCursor;
 END;
end; { CreateDOSProcessRedirected }

procedure TForm1.Button1Click(Sender: TObject);
begin
 // Pour cet exemple le fichier input.txt contient uniquement un
 // retour chariot ( ligne vide) qui simulera la frappe de la touche entrée.
if CreateDOSProcessRedirected('CMD /C Date','C:\Temp\InPut.txt',
                           'C:\Temp\OutPut.txt',
                           '')
 then MessageDlg('Fonction terminée avec succés.', mtInformation, [mbOK], 0);                           
{ // Pour Windows98
 CreateDOSProcessRedirected('Command /C Date','C:\Temp\InPut.txt',
                           'C:\Temp\OutPut.txt',
                           '')
}

end;

end.
