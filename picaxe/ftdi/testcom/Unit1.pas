unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Registry;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo2: TMemo;
    ListBox1: TListBox;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }

  public
    { Déclarations publiques }

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}



procedure TForm1.Button1Click(Sender: TObject);
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


procedure TForm1.FormCreate(Sender: TObject);
begin
self.Button1Click(self);
end;

end.
