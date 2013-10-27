unit Unit1Mini;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ExtCtrls, OleCtrls, ShockwaveFlashObjects_TLB, tabassoc,
  ComCtrls;
const
   max=50;
   max_char=72;

type
  TForm21 = class(TForm)

    MainMenu1: TMainMenu;
    Fichier1: TMenuItem;
    Nouveau1: TMenuItem;
    Enregistrersous1: TMenuItem;
    Enregistrer1: TMenuItem;
    Quitter1: TMenuItem;
    Edition1: TMenuItem;
    Ajoutertape1: TMenuItem;
    Supprimertape1: TMenuItem;
    Simulation1: TMenuItem;
    Dmarrer1: TMenuItem;
    Stopper1: TMenuItem;
    Paramtres1: TMenuItem;
    Timer1: TTimer;
    Panel1: TPanel;
    chkA: TCheckBox;
    chkB: TCheckBox;
    chkC: TCheckBox;
    chkD: TCheckBox;
    chkE: TCheckBox;
    chkF: TCheckBox;
    chkH: TCheckBox;
    chkG: TCheckBox;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    Ouvrir1: TMenuItem;
    Timer2: TTimer;
    flash1: TShockwaveFlash;
    Associeruneanimationflash1: TMenuItem;
    fscmd: TListBox;
    Memo1: TMemo;
    procedure Quitter1Click(Sender: TObject);
    procedure Ajoutertape1Click(Sender: TObject);
    procedure etapeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Supprimertape1Click(Sender: TObject);
    procedure etapeDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Nouveau1Click(Sender: TObject);

    procedure etapeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Dmarrer1Click(Sender: TObject);
    procedure Stopper1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Ouvrir1Click(Sender: TObject);
    procedure Enregistrersous1Click(Sender: TObject);
    procedure Enregistrer1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Associeruneanimationflash1Click(Sender: TObject);
    procedure flash1FSCommand(ASender: TObject; const command,
      args: WideString);
    procedure DetecterleportassociaucableAXE0271Click(Sender: TObject);
    procedure DtecterlecbleUSBPICAXE1Click(Sender: TObject);
    procedure Gnrerunfichierbasic1Click(Sender: TObject);
    procedure ConstruireunfichierbasicPICAXE1Click(Sender: TObject);
  private
    { Déclarations privées }

    mini,maxi,frame:integer;
    //tempo:int64;
    dico:TableauAssociatif;
    filename:string;
    ordres:array[0..50] of string;
    tempo:array[0..50] of longint;//compte en ms
    step_count:integer;
    comp_count:integer;
    debut,fin,deltaX,deltaY:integer;
    liens, receptivites:array[0..max,0..max] of string;
    etapes_actives:array[0..max] of integer;
    procedure gerer_ordres();
    procedure clean;
    procedure redraw;
    function equations(eq:string):string;
    procedure drawlink(d,f:integer);
    procedure save(fn:string);
    function traduit(instr:string;mode:string='cond'):string;
    function clean_instr(instr:string):string;
  public
    { Déclarations publiques }
    ftdi_device: string;
    com_port: string;
    picaxe:string;
  end;

var
  Form21: TForm21;
  flash:string;

implementation

uses Unit2Mini, Unit3Mini, unit4Mini, progr1Mini;
{$R *.dfm}
function Split(chaine : String; delimiteur : string) : TStringList;
var
L : TStringList;
begin
  L:=TStringList.create;
  L.text := StringReplace(chaine, delimiteur, #13#10, [rfReplaceAll]);
  Split:=L;
end;


function TForm21.clean_instr(instr:string):string;
var s:string;
    ar:Tstringlist;
begin
   s:=instr;
   if (pos('and',s)<>0) and (pos('or',s)<> 0) then begin
        //instr complexe à scinder
        ar:=split(s,'or');
   end;
   result:=s;
end;
procedure TForm21.gerer_ordres();
var i,j:integer;
    cmd:string;
    delta:integer;
begin
if flash='' then exit;
//modume simple allumage

if pos('FEUX1',uppercase(flash)) <> 0  then begin
self.flash1.Loop:=true;
self.flash1.Play;
for i:=0 to step_count do begin
cmd:=trim(uppercase(self.ordres[i]));
if pos('F1V',cmd) <> 0 then self.flash1.SetVariable('Feu1','V');
if pos('F1R',cmd) <> 0 then self.flash1.SetVariable('Feu1','R');
if pos('F10',cmd) <> 0 then self.flash1.SetVariable('Feu1','O');
if pos('F2V',cmd) <> 0 then self.flash1.SetVariable('Feu2','V');
if pos('F2R',cmd) <> 0 then self.flash1.SetVariable('Feu2','R');
if pos('F20',cmd) <> 0 then self.flash1.SetVariable('Feu2','O');
if pos('F3V',cmd) <> 0 then self.flash1.SetVariable('Feu3','V');
if pos('F3R',cmd) <> 0 then self.flash1.SetVariable('Feu3','R');
if pos('F30',cmd) <> 0 then self.flash1.SetVariable('Feu3','O');
//flash1.GotoFrame(1);
//flash1.GotoFrame(0);
end;
exit;
end;

if pos('VA_ET_VIENT',uppercase(flash)) <> 0  then begin
self.flash1.Loop:=true;
for i:=0 to step_count do begin

if dico.EstPresent('inter1') then begin self.flash1.SetVariable('Inter1',dico['inter1']); self.flash1.Play;  end;
if dico.EstPresent('inter0') then begin self.flash1.SetVariable('Inter0',dico['inter0']); self.flash1.Play;  end;
if dico.Keys.Count=0 then
begin
self.flash1.SetVariable('Inter0','0');
self.flash1.Play;
self.flash1.SetVariable('Inter1','0');
self.flash1.Play;
dico['inter0']:='0';
dico['inter1']:='0';
end;

cmd:=trim(uppercase(self.ordres[i]));
if (cmd='') then continue;
self.flash1.SetVariable('Lampe','0');
self.flash1.Play;
if pos('ALLUMER',cmd) <> 0 then begin self.flash1.setVariable('Lampe','1'); self.flash1.Play;  end;
if pos('ETEINDRE',cmd) <> 0 then begin self.flash1.setVariable('Lampe','0'); self.flash1.Play; end;
end;
exit;
end;

if pos('FONCTION',uppercase(flash)) <> 0  then begin
self.flash1.Loop:=true;
for i:=0 to step_count do begin
if dico.EstPresent('inter') then begin self.flash1.SetVariable('Inter',dico['inter']); self.flash1.Play;  end;
cmd:=trim(uppercase(self.ordres[i]));
if (cmd='') then continue;
self.flash1.SetVariable('Lampe','0');
self.flash1.Play;
if pos('ALLUMER',cmd) <> 0 then begin self.flash1.setVariable('Lampe','1'); self.flash1.Play;  end;
if pos('ETEINDRE',cmd) <> 0 then begin self.flash1.setVariable('Lampe','0'); self.flash1.Play; end;
end;

exit;
end;
//module ascenseur
  if pos('ASCENSEUR',uppercase(flash)) <> 0  then begin
    for i:=0 to step_count do begin
      cmd:=trim(uppercase(self.ordres[i]));
      if (cmd='') then continue;
      //memo1.lines.Add(cmd);
      delta:=1;
      //delta := (self.tempo div 50) mod 2;

      if pos('MONTER',cmd) <> 0 then
         begin
          self.maxi:=20;
            for j:=0 to step_count do begin
               if pos('niveau1',liens[i,j]) <> 0 then self.maxi:=10;
            end;
          if self.frame<self.maxi then self.frame:=self.frame+delta;
          self.flash1.GotoFrame(self.frame);
         end;
      if pos('DESCENDRE',cmd) <> 0 then
         begin
           self.mini:=0;
           for j:=0 to step_count do begin
             if pos('niveau1',liens[i,j]) <> 0 then self.mini:=10;
           end;
           if self.frame>self.mini then self.frame:=self.frame-delta;
           self.flash1.GotoFrame(self.frame);
       end;
    end;
    self.Update;
  end;


end;

procedure TForm21.save(fn:string);
var f:textfile;
    i,j,k:integer;

begin
     assignfile(f,fn);
     rewrite(f);
     writeln(f,self.step_count-1);
     for i:=0 to self.step_count-1 do begin
       writeln(f,'@');
       for k:=0 to (self.Components[self.comp_count+i] as Tmemo).Lines.Count-1 do
         writeln(f,(self.Components[self.comp_count+i] as Tmemo).Lines[k]);
       writeln(f,'#');
       writeln(f,(self.Components[self.comp_count+i] as Tmemo).Top);
       writeln(f,(self.Components[self.comp_count+i] as Tmemo).Left);

     end;
     for i:=0 to self.step_count-1 do
     for j:=0 to self.step_count-1 do
       writeln(f,liens[i,j]);
     writeln(f,flash);
     closefile(f);
end;

function TForm21.traduit(instr:string;mode:string='cond'):string;
var s:string;
begin
s:=uppercase(instr);

if (mode='cond') then begin
s  := Trim(StringReplace(s,'.', ' and ', [rfReplaceAll, rfIgnoreCase]));
s  := Trim(StringReplace(s,'+', ' or ', [rfReplaceAll, rfIgnoreCase]));
s  := Trim(StringReplace(s,'/INTER1', 'pinC.7=0', [rfReplaceAll]));
s  := Trim(StringReplace(s,'/INTER2', 'pinC.6=0', [rfReplaceAll]));
s  := Trim(StringReplace(s,'INTER1', 'pinC.7=1', [rfReplaceAll]));
s  := Trim(StringReplace(s,'INTER2', 'pinC.6=1', [rfReplaceAll]));
s  := Trim(StringReplace(s,'/INTER', 'pinC.7=0', [rfReplaceAll, rfIgnoreCase]));
s  := Trim(StringReplace(s,'INTER', 'pinC.7=1', [rfReplaceAll, rfIgnoreCase]));
result:=s;
exit;
end;

if (mode='ordre') then begin
if pos('ALLUMER',s) <>0 then  begin result:='high B.0'; exit; end;
if pos('ETEINDRE',s) <>0 then begin result:='low B.0'; exit; end;
result:=';';
end;

end;

function TForm21.equations(eq:string):string;
var s,add:string;
kk:integer;
begin



    s := Trim(eq);

    for kk:=0 to dico.keys.Count-1 do begin
       if pos(dico.Keys[kk],s)<>0  then s  := Trim(StringReplace(s,dico.Keys[kk],trim(dico[dico.Keys[kk]]) , [rfReplaceAll, rfIgnoreCase]));
    end;

    For kk := 65 To max_char do
    begin
    if Tcheckbox(FindComponent('chk'+chr(kk))).Checked then add:='1' else add:='0';
    s  := Trim(StringReplace(s,Chr(kk), add, [rfReplaceAll, rfIgnoreCase]));
    s  := Trim(StringReplace(s,'/0', '1', [rfReplaceAll, rfIgnoreCase]));
    s  := Trim(StringReplace(s,'/1', '0', [rfReplaceAll, rfIgnoreCase]));
    s  := Trim(StringReplace(s,'0.0', '0', [rfReplaceAll, rfIgnoreCase]));
    s  := Trim(StringReplace(s,'0.1', '0', [rfReplaceAll, rfIgnoreCase]));
    s  := Trim(StringReplace(s,'1.0', '0', [rfReplaceAll, rfIgnoreCase]));
    s  := Trim(StringReplace(s,'1.1', '1', [rfReplaceAll, rfIgnoreCase]));
    s  := Trim(StringReplace(s,'0+0', '0', [rfReplaceAll, rfIgnoreCase]));
    s  := Trim(StringReplace(s,'0+1', '1', [rfReplaceAll, rfIgnoreCase]));
    s  := Trim(StringReplace(s,'1+0', '1', [rfReplaceAll, rfIgnoreCase]));
    s  := Trim(StringReplace(s,'1+1', '1', [rfReplaceAll, rfIgnoreCase]));
    s  := Trim(StringReplace(s,'0~0', '0', [rfReplaceAll, rfIgnoreCase]));
    s  := Trim(StringReplace(s,'0~1', '1', [rfReplaceAll, rfIgnoreCase]));
    s  := Trim(StringReplace(s,'1~0', '1', [rfReplaceAll, rfIgnoreCase]));
    s  := Trim(StringReplace(s,'1~1', '0', [rfReplaceAll, rfIgnoreCase]));
    end;
    equations:=s;
end;
procedure TForm21.redraw;
var i,j:integer;
begin
form21.Refresh;
for i:=0 to step_count-1 do
for j:=0 to step_count-1 do
if liens[i,j]<>'-1' then self.drawlink(i,j);
end;

procedure TForm21.drawlink(d,f:integer);
var haut1,gauche1,haut2,gauche2,milieu:integer;

begin
if d=f then exit;
if d=-1 then exit;
if f=-1 then exit;

if liens[d,f]='-1' then exit;

gauche1:=(self.Components[self.comp_count+d] as Tmemo).Left + (self.Components[self.comp_count+d] as Tmemo).Width div 2;
haut1:=(self.Components[self.comp_count+d] as Tmemo).Top + (self.Components[self.comp_count+d] as Tmemo).Height div 2;

gauche2:=(self.Components[self.comp_count+f] as Tmemo).Left + (self.Components[self.comp_count+f] as Tmemo).Width div 2;
haut2:=(self.Components[self.comp_count+f] as Tmemo).Top + (self.Components[self.comp_count+f] as Tmemo).Height div 2;

milieu:=(haut1+haut2) div 2;

form21.Canvas.MoveTo(gauche1,haut1);
form21.Canvas.LineTo(gauche2,haut1);
form21.Canvas.LineTo(gauche2,milieu);
if (haut1>haut2) then begin
form21.Canvas.LineTo(gauche2-10,milieu+10);
form21.Canvas.MoveTo(gauche2+10,milieu+10);

end else begin
form21.Canvas.LineTo(gauche2-10,milieu-10);
form21.Canvas.MoveTo(gauche2+10,milieu-10);

end;
form21.Canvas.LineTo(gauche2,milieu);
form21.Canvas.LineTo(gauche2,haut2);

form21.Canvas.Textout(gauche2+10,milieu,liens[d,f]);

end;

procedure TForm21.clean;
var i,j:integer;
begin
self.Refresh;
for i:=0 to max do
for j:=0 to max do
self.liens[i,j]:='-1';

if step_count=0 then exit;
repeat
self.Components[self.ComponentCount-1].Destroy;
dec(step_count);
until (self.ComponentCount = self.comp_count);
end;



procedure TForm21.Quitter1Click(Sender: TObject);
begin
form21.Close;
end;

procedure TForm21.Ajoutertape1Click(Sender: TObject);
var txt:Tmemo;
begin
txt:=Tmemo.create(self);
txt.Parent :=self;
txt.Visible:=true;
txt.Color :=clYellow;
txt.Text:='Etape '+inttostr(self.step_count);
if self.step_count=0 then txt.Top:=30 else txt.Top:=30+self.step_count*60;
txt.left:=110;
txt.Height:=40;
txt.Width:=120;
txt.tag:=step_count;
txt.ScrollBars:=ssVertical;
inc(step_count);
txt.OnMouseDown:=etapeMouseDown;
txt.OnDragOver:=etapeDragOver;
txt.OnDragDrop:=etapeDragDrop;
end;

procedure TForm21.etapeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
//form1.Caption:=inttostr((sender as Tmemo).tag);
(sender as Tmemo).BeginDrag(false);
debut:=(sender as Tmemo).Tag;
deltaX:=(sender as Tmemo).Left;
deltaY:=(sender as Tmemo).Top;
end;

procedure TForm21.FormDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
Accept := (Source is Tmemo);

end;

procedure TForm21.FormDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
//(source as tmemo).Top := Y-self.deltaY;
//(source as tmemo).Left := X-self.deltaX;
(source as tmemo).Top := Y;
(source as tmemo).Left := X;
self.redraw;
end;

procedure TForm21.FormCreate(Sender: TObject);
var i,j:integer;
begin
form21.picaxe:='18m2';
self.comp_count:=self.ComponentCount;
for i:=0 to self.comp_count do
for j:=0 to self.comp_count do
self.liens[i,j]:='-1';
dico:=TableauAssociatif.Create;
end;

procedure TForm21.Supprimertape1Click(Sender: TObject);
var i,j:integer;
begin
if self.ComponentCount = self.comp_count then exit;
self.Components[self.ComponentCount-1].Destroy;
for i:=0 to self.comp_count do self.liens[i,self.step_count-1]:='-1';
for j:=0 to self.comp_count do self.liens[self.step_count-1,j]:='-1';
dec(self.step_count);
self.redraw;

end;

procedure TForm21.etapeDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);

begin
accept:=(source is Tmemo);
end;

procedure TForm21.Nouveau1Click(Sender: TObject);
begin
timer1.Enabled:=false;
self.clean;
self.filename:='';
self.flash1.Visible:=false;
flash:='';
self.fscmd.Clear;
end;



procedure TForm21.etapeDragDrop(Sender, Source: TObject; X, Y: Integer);
var value:string;
begin
debut:=(source as Tmemo).Tag;
fin:=(sender as Tmemo).Tag;
//form1.caption:=inttostr(debut)+'-'+inttostr(fin);
if (debut=fin) then begin
  self.debut:=-1;
  self.fin:=-1;
  exit;
end;
value := InputBox('Test program', 'Entrez la condition de transition de l''etape '+inttostr(debut)+' à l''étape '+inttostr(fin), liens[debut,fin]);
if value='' then value:='-1';
self.liens[debut,fin]:=value;
self.drawlink(self.debut,self.fin);
self.redraw;
self.debut:=-1;
self.fin:=-1;

end;

procedure TForm21.Dmarrer1Click(Sender: TObject);
var i,j:integer;
begin

self.Stopper1Click(self);
self.frame:=0;

memo1.lines.clear;
if step_count=0 then exit;
for i:=0 to step_count do
for j:=0 to step_count do
receptivites[i,j]:='-1';

(self.Components[self.comp_count] as Tmemo).Color:=TColor($3378E4);
timer1.Enabled:=true;
etapes_actives[0]:=1;
for i:=1 to max do etapes_actives[i]:=-1;

end;

procedure TForm21.Stopper1Click(Sender: TObject);
var i,j:integer;
begin
self.flash1.GotoFrame(0);
timer1.Enabled:=false;
for i:=0 to max do begin etapes_actives[i]:=-1; self.tempo[i]:=0;  end;

for i:=0 to step_count-1 do (self.Components[self.comp_count+i] as Tmemo).Color:=clYellow;

for i:=0 to step_count do
for j:=0 to step_count do
receptivites[i,j]:='-1';

end;

procedure TForm21.Timer1Timer(Sender: TObject);

var cond:string;
    i,j,k,kk:integer;
begin

    for i:=0 to step_count do begin
       if etapes_actives[i]=1 then for j:=0 to step_count do begin
          cond:=liens[i,j];
          if cond<>'-1' then begin
             //cond a 1
             if cond='1' then receptivites[i,j]:='1' else receptivites[i,j]:='-1';
             //Receptivites checkbox
             For k := 65 to max_char do
             begin
             If (cond = Chr(k)) Then if Tcheckbox(FindComponent('chk'+chr(k))).Checked  Then receptivites[i, j] := '1' Else receptivites[i, j] := '-1';
             //If (cond = '/'+Chr(k)) Then if Tcheckbox(FindComponent('chk'+chr(k))).Checked  Then receptivites[i, j] := '-1' Else receptivites[i, j] := '1';
             If (Pos('/', cond)<>0) or (Pos('+', cond)<>0) or (Pos('~', cond)<>0) or (Pos('.', cond)<>0) then receptivites[i, j]:=equations(cond);

             for kk:=0 to dico.keys.Count-1 do begin
                 if pos(dico.Keys[kk],cond)<>0  then receptivites[i, j]:=equations(cond);
             end;


             end;
          end;
       end;

       //traiter les receptivites

       //for ii:=0 to step_count do
       For j:=0 to step_count do
       If (etapes_actives[i] = 1) and (trim(receptivites[i, j]) = '1') Then begin

              etapes_actives[i] := -1;
              (self.Components[self.comp_count+i] as Tmemo).Color:=clYellow;
              self.ordres[i]:='';
              self.tempo[i]:=0;

              etapes_actives[j] := 1;
              (self.Components[self.comp_count+j] as Tmemo).Color:=TColor($3378E4);
              self.tempo[j]:=0;
              self.ordres[j]:=(self.Components[self.comp_count+j] as Tmemo).Text;
              //memo1.lines.Add('unset '+inttostr(i)+' set '+inttostr(j)+' '+self.ordres[j]);




       end; //if
       gerer_ordres();
       self.tempo[i]:=self.tempo[i]+self.Timer1.Interval;
    end;//for
end;

procedure TForm21.Ouvrir1Click(Sender: TObject);
var f:textfile;
s,txt:string;
i,j,cnt:integer;
begin
//memo1.Clear;
timer1.Enabled:=false;
dico.fElems.Clear;
self.fscmd.Clear;
self.OpenDialog1.InitialDir:=GetCurrentDir()+'\GPH\';
self.OpenDialog1.FileName:='*.GPH';
self.OpenDialog1.Filter:='*.GPH';
if self.OpenDialog1.Execute then begin
   if self.OpenDialog1.Filename='*.GPH' then exit;
   //form1.caption:=self.OpenDialog1.FileName;
   self.filename:=self.OpenDialog1.FileName;
   self.clean;
   assignfile(f,self.OpenDialog1.filename);
   reset(f);
   //lit nb etapes
   readln(f,s);
   cnt:=strtoint(trim(s));
   //lit @
   readln(f,s);
   //lit le texte
   readln(f,txt);
   while s<>'#' do begin
       readln(f,s);
       if (s<>'#') then txt:=txt+chr(13)+chr(10)+s;
   end;
   //Ajouter etape 0
   Ajoutertape1Click(sender);
   (self.Components[self.comp_count] as Tmemo).Text:=txt;
   readln(f,s); (self.Components[self.comp_count] as Tmemo).Top:=StrToint(trim(s));
   readln(f,s); (self.Components[self.comp_count] as Tmemo).Left:=StrToint(trim(s));
   //
   for i:=1 to cnt do begin
      readln(f,s);
      readln(f,txt);
      while s<>'#' do begin
       readln(f,s);
       if (s<>'#') then txt:=txt+chr(13)+chr(10)+s;
      end;
      Ajoutertape1Click(sender);
      (self.Components[self.comp_count+i] as Tmemo).Text:=txt;
      readln(f,s); (self.Components[self.comp_count+i] as Tmemo).Top:=StrToint(trim(s));
      readln(f,s); (self.Components[self.comp_count+i] as Tmemo).Left:=StrToint(trim(s));

   end;

   for i:=0 to cnt do
     for j:=0 to cnt do
       begin
         readln(f,s);
         //memo1.Lines.Add(s);
         self.liens[i,j]:=trim(s);
       end;
   readln(f,s);

   flash:=s;

   if flash<>'' then begin
      self.flash1.movie:=GetCurrentDir+chr(92)+s;
      self.flash1.Visible:=true;
      self.flash1.Loop:=false;
      //self.TrackBar1.Max:=self.flash1.TotalFrames-1;
   end else begin
      self.flash1.Visible:=false;
   end;
   closefile(f);
   self.redraw;
 end;//if


end;

procedure TForm21.Enregistrersous1Click(Sender: TObject);
begin
  self.SaveDialog1.InitialDir:=GetCurrentdir+'\GPH\';
  self.SaveDialog1.FileName:='*.GPH';
  self.SaveDialog1.Filter:='*.GPH';
  if self.SaveDialog1.Execute then begin
     self.Save(self.SaveDialog1.FileName);
  end;
end;

procedure TForm21.Enregistrer1Click(Sender: TObject);
var test:integer;
begin
 if self.filename='' then exit;
 if self.filename='*.GPH' then exit;
 self.SaveDialog1.FileName:=self.filename;
  self.SaveDialog1.Filter:='*.GPH';
  //if self.SaveDialog1.Execute then begin
     self.Save(self.FileName);
     test:=MessageDlg('Sauvegarde',mtInformation, mbOKCancel, 0);
  //end;
end;

procedure TForm21.FormShow(Sender: TObject);
begin
redraw;
end;

procedure TForm21.FormResize(Sender: TObject);
begin
redraw;
end;

procedure TForm21.Timer2Timer(Sender: TObject);
begin
redraw;

end;

procedure TForm21.Associeruneanimationflash1Click(Sender: TObject);
begin
form22.show;
end;

procedure TForm21.flash1FSCommand(ASender: TObject; const command,
  args: WideString);
  var i:integer;
  begin

//memo1.Lines.Add(command+' '+args);
dico[command]:=args;
fscmd.Clear;
//dico.
  for i:=0 to dico.Keys.Count-1 do
  begin
    fscmd.Items.Add(dico.Keys[i]);
    if trim(dico[dico.Keys[i]]) = '1' then begin
        fscmd.Selected[i] :=true;
    end;
  end;
end;

procedure TForm21.DetecterleportassociaucableAXE0271Click(Sender: TObject);
begin
form23.show;
form23.Edit1.Text:=self.com_port;
end;

procedure TForm21.DtecterlecbleUSBPICAXE1Click(Sender: TObject);
begin
form24.Timer1.Enabled:=true;
form24.show;
end;

procedure TForm21.Gnrerunfichierbasic1Click(Sender: TObject);
begin
if self.step_count=0 then exit;
frmProg2.show;
end;

procedure TForm21.ConstruireunfichierbasicPICAXE1Click(Sender: TObject);

const max_input=5;
const max_output=8;
var i,j:integer;
ordre:string;
begin
if self.step_count=0 then exit;
frmProg2.SynMemo1.Clear;
//lister les entrees <>
//lister les sorties <>
//affecter si possible les pins  aux entrees sorties
//proposer le prog
for i:=0 to step_count-1 do begin
  frmProg2.SynMemo1.lines.Add('clear'+inttostr(i)+':   let pinsB=%00000000');
  ordre:=Tmemo(self.Components[self.comp_count+i]).Text;
  frmProg2.SynMemo1.lines.Add('etape'+inttostr(i)+':   '+self.traduit(ordre,'ordre'));
  frmProg2.SynMemo1.lines.add('          pause 100');
  for j:=0 to step_count-1 do
  begin
    if (i=j) then continue;
    if self.liens[i,j] <> '-1' then begin
       frmProg2.SynMemo1.lines.Add(self.clean_instr('          if '+self.traduit(liens[i,j],'cond')+' then goto clear'+inttostr(j)));
    end;
  end;
  frmProg2.SynMemo1.lines.Add('          goto etape'+inttostr(i));
end;
//frmProg.SynMemo1.Text:=self.memo1.Text;
Gnrerunfichierbasic1Click(sender);
end;

end.
