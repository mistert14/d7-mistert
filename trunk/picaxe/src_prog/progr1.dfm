object Form1: TForm1
  Left = 198
  Top = 124
  Width = 366
  Height = 411
  Caption = 'Programmation du PICAXE'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 112
    Top = 336
    Width = 121
    Height = 33
    Caption = 'Programmer le PICAXE'
    TabOrder = 0
    Visible = False
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 350
    Height = 329
    Align = alTop
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Button2: TButton
    Left = 112
    Top = 336
    Width = 121
    Height = 33
    Caption = 'Programmer le PICAXE'
    TabOrder = 2
    OnClick = Button2Click
  end
end
