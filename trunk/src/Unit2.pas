unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OleCtrls, ShockwaveFlashObjects_TLB;

type
  TForm2 = class(TForm)
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
  Form2: TForm2;

implementation

uses Unit1;

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
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

procedure TForm2.Button3Click(Sender: TObject);
begin
self.Hide;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
form1.flash1.Movie:=self.ShockwaveFlash1.movie;
flash:= self.ShockwaveFlash1.movie;
flash  := Trim(StringReplace(flash,GetCurrentDir+'\', '', [rfReplaceAll, rfIgnoreCase]));
//form1.Caption:=flash;
form1.flash1.Loop:=false;
form1.flash1.Enabled:=true;
form1.flash1.Visible:=true;
form2.Hide;
end;

end.
