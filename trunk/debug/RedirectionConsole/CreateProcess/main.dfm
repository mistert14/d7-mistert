object Form1: TForm1
  Left = 357
  Top = 237
  Width = 388
  Height = 384
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 184
    Top = 312
    Width = 75
    Height = 25
    Caption = 'Ex'#195#169'cute'
    TabOrder = 0
    OnClick = Button1Click
  end
  object BitBtn1: TBitBtn
    Left = 272
    Top = 312
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkClose
  end
  object Memo1: TMemo
    Left = 2
    Top = 4
    Width = 352
    Height = 124
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object Memo2: TMemo
    Left = 1
    Top = 135
    Width = 350
    Height = 138
    Lines.Strings = (
      'Memo2')
    TabOrder = 3
  end
end
