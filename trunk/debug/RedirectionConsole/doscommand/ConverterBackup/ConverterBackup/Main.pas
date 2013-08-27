unit Main;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{ Programme de démonstration du composant TDosCommand version d'origine
  http://Laurent-Dardenne.Developpez.com

 Comment l'utiliser ce composant :
 ---------------
  - Saisissez votre commande dans la propriété 'CommandLine'
  - executez the processus avec la méthode 'Execute'
  - Si vous souhaitez arrêter le processus avant sa fin normal, utilisez la méthode 'Stop'
  - Si vous souhaitez que le processus s'arrête après XXX seconde d'activité,
    utilisez la propriété 'MaxTimeAfterBeginning'
  - Si vous souhaitez que le processus s'arrête après XXX seconde sans donnée reçue en entrée,
    utilisez la propriété 'MaxTimeAfterLastOutput'
  - Pour rediriger directement la sortie du process vers un composant memo ou un richedit, ...
    utilisez la propriété 'OutputLines',
    exemple : DosCommand1.OutputLnes := Memo1.Lines;
  - vous pouvez accéder à toutes les sorties de la dernière commande par la propriété 'Lines' ;
  - vous pouvez changer la priorité du processus avec la propriété 'Priority';
    la valeur de la priorité doit être une de ces valeurs :
     [HIGH_PRIORITY_CLASS, IDLE_PRIORITY_CLASS,NORMAL_PRIORITY_CLASS, REALTIME_PRIORITY_CLASS]
  - vous pouvez recevoir un événement pour chaque nouvelle ligne et pour la
    fin du processus avec les événements suivants :
      procedure OnNewLine(Sender: TObject; NewLine: string;OutputType: TOutputType);'
      procedure OnTerminated(Sender: TObject);
  - vous pouvez envoyer des données en entrée au processus DOS via la méthode
    SendLine(Value : corde ; Eol : Boolean);
    Eol indique si le programme doit ajouter un CR/LF en fin de chaîne.

 Création du composant et convertion du code par Maxime_collomb@yahoo.fr .
 Code d'origine sur Community.borland.com :
  http://www.vmlinux.org/jakov/community.borland.com/10387.html
}

interface

uses
{$IFNDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, DosCommand;

type
  TForm1 = class(TForm)
    DosCommand1: TDosCommand;
    Memo1: TMemo;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Button1: TButton;
    procedure BitBtn1Click(Sender: TObject);
    procedure DosCommand1NewLine(Sender: TObject; NewLine: String;
      OutputType: TOutputType);
    procedure DosCommand1Terminated(Sender: TObject);
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

procedure TForm1.BitBtn1Click(Sender: TObject);
const Ajout_CRLF:Boolean=True;

Var  MaCommande :String;

begin
   // Exécute une nouvelle instance de l'interpréteur de commandes
   // Command.exe pour W98
 DosCommand1.CommandLine :='Cmd.exe';

  // Redirige la sortie du programme console vers un composant d'affichage TMemo ou autre
  // Dans cet exemple on utilise l'evénement OnNewLine
 //DosCommand1.OutputLines := Memo1.Lines;

  //Lance le programme console
 DosCommand1.Execute;

  // équivalent à la touche entrée
 DosCommand1.SendLine('', Ajout_CRLF);

  // Instruction pour le programme console
 MaCommande :='Ping 213.09.25.36';

  // Insére et exécute l'instruction contenue dans la chaîne
 DosCommand1.SendLine(MaCommande, Ajout_CRLF);
end;

procedure TForm1.DosCommand1NewLine(Sender: TObject; NewLine: String;
  OutputType: TOutputType);
begin
 Case OutputType of
  otEntireLine:  Memo1.Lines.Add(NewLine);
  otBeginningOfLine :Memo1.Lines.Add('** Nouvelle ligne reçue **');
 end;
end;

procedure TForm1.DosCommand1Terminated(Sender: TObject);
 // La version amélioré permet de recevoir le code de sortie du process.
begin
 Memo1.Lines.Add('** Processus terminé **');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // on termine proprement l'instance de l'interpréteur de commandes
 DosCommand1.SendLine('Exit', True);
end;

end.
