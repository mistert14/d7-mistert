unit Unit3Mini;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Registry, ExtCtrls;

type
  TForm23 = class(TForm)
    Button1: TButton;
    Memo2: TMemo;
    ListBox1: TListBox;
    Button2: TButton;
    Timer1: TTimer;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { Déclarations privées }

  public
    { Déclarations publiques }

  end;

var
  Form23: TForm23;

implementation

uses Unit1Mini;

{$R *.dfm}



procedure TForm23.Button1Click(Sender: TObject);
var reg:Tregistry;
    st1,st2:Tstrings;
    i:integer;
begin
  self.ListBox1.Clear;
    Reg:=Tregistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey('hardware\devicemap\serialcomm',false);
    //reg.OpenKey('SYSTEM\ControlSet001\Enum\FTDIBUS',false);
    st1:=TstringList.create;
    st2:=TstringList.create;
    try
      //reg.GetKeyNames(st1);
      reg.GetValueNames(st1);
      for i:=0 to st1.Count -1 do
         self.ListBox1.items.Add(reg.ReadString(st1.Strings[i]));
        //self.ListBox1.items.Add(reg.ReadString(st1.Strings[0]+' '+st2.Strings[i]));
    finally
      st1.Free;
      st2.free;
    end;
    reg.CloseKey;
  finally
    reg.Free;
  end;

end;


procedure TForm23.FormCreate(Sender: TObject);
begin
self.Button1Click(self);
end;

procedure TForm23.Button2Click(Sender: TObject);
var i:integer;
begin
self.Hide;
if self.Edit1.Text <> '' then Form21.com_port:=self.Edit1.text;
self.Timer1.Enabled:=false;
//form1.caption:=form1.com_port;
end;

procedure TForm23.Timer1Timer(Sender: TObject);
begin
self.Button1Click(self);
end;

procedure TForm23.FormShow(Sender: TObject);
begin
self.Timer1.Enabled:=true;
end;

procedure TForm23.ListBox1Click(Sender: TObject);
var i:integer;
begin
if self.ListBox1.Items.count=0 then edit1.Text;
for i:=0 to self.ListBox1.items.count-1 do
if self.ListBox1.Selected[i] then edit1.Text:=self.ListBox1.Items[i];
end;

end.
