object Form3: TForm3
  Left = 329
  Top = 232
  Width = 425
  Height = 383
  AutoSize = True
  Caption = 'D'#233'tecter le port du cable USB AXE027'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 312
    Width = 161
    Height = 25
    Caption = 'Lister les ports s'#233'rie ouverts'
    TabOrder = 0
    Visible = False
    OnClick = Button1Click
  end
  object Memo2: TMemo
    Left = 96
    Top = 0
    Width = 313
    Height = 313
    Enabled = False
    Lines.Strings = (
      'Comment utiliser cet outil ?'
      ''
      'Cet outil vous permet de lister l'#39'ensemble des '
      'ports s'#233'ries ouverts sur'
      'votre machine. Testez le dans deux cas:'
      ''
      '-Sans le cable USB AXE027'
      '- Avec le cable USB AXE 027'
      ''
      'Par comparaison vous trouverez le port ouvert.')
    TabOrder = 1
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 89
    Height = 305
    Align = alCustom
    ItemHeight = 13
    TabOrder = 2
    OnClick = ListBox1Click
  end
  object Button2: TButton
    Left = 264
    Top = 320
    Width = 145
    Height = 25
    Caption = 'Utiliser ce port'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 200
    Top = 320
    Width = 57
    Height = 21
    Enabled = False
    TabOrder = 4
  end
  object Timer1: TTimer
    Interval = 5000
    OnTimer = Timer1Timer
    Left = 168
    Top = 312
  end
end
