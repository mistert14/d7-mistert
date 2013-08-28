object Form1: TForm1
  Left = 198
  Top = 124
  Width = 457
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
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 24
    Top = 320
    Width = 161
    Height = 25
    Caption = 'Lister les ports s'#233'rie ouverts'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo2: TMemo
    Left = 208
    Top = 0
    Width = 233
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
    Top = 8
    Width = 201
    Height = 305
    ItemHeight = 13
    TabOrder = 2
  end
  object Button2: TButton
    Left = 232
    Top = 320
    Width = 145
    Height = 25
    Caption = 'Utiliser ce port'
    TabOrder = 3
  end
end
