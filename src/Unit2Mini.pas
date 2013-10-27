unit Unit2Mini;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OleCtrls, ShockwaveFlashObjects_TLB;

type
  TForm22 = class(TForm)
    ShockwaveFlash1: TShockwaveFlash;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form22: TForm22;

implementation

uses Unit1Mini;

{$R *.dfm}

procedure TForm22.Button1Click(Sender: TObject);
begin
self.OpenDialog1.InitialDir:=GetCurrentdir;
self.OpenDialog1.FileName:='*.swf';
self.OpenDialog1.Filter:='*.swf';
if self.OpenDialog1.Execute then begin
   self.ShockwaveFlash1.Movie:=self.OpenDialog1.FileName;
   self.ShockwaveFlash1.Loop:=true;
   self.ShockwaveFlash1.Playing:=true;
   self.ShockwaveFlash1.Rewind;
   self.ShockwaveFlash1.Play;
end;
end;

procedure TForm22.Button3Click(Sender: TObject);
begin
self.Hide;
end;

procedure TForm22.Button2Click(Sender: TObject);
begin
form21.flash1.Movie:=self.ShockwaveFlash1.movie;
flash:= self.ShockwaveFlash1.movie;
flash  := Trim(StringReplace(flash,GetCurrentDir+'\', '', [rfReplaceAll, rfIgnoreCase]));
//form1.Caption:=flash;
form21.flash1.Loop:=false;
form21.flash1.Enabled:=true;
form21.flash1.Visible:=true;
form22.Hide;
end;

end.
